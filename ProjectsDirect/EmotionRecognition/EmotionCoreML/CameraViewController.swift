//
//  CameraViewController.swift
//  Project Emotion Recognition
//
//  Created by Michael Guel on 11/13/24.
//

import UIKit
import AVFoundation

class CameraViewController: UIViewController {

    // MARK: - IBOutlets
    @IBOutlet weak var previewView: UIView!
    @IBOutlet weak var capturePhotoButton: UIButton!
    @IBOutlet weak var recordVideoButton: UIButton!

    // MARK: - Properties
    var captureSession: AVCaptureSession?
    var videoPreviewLayer: AVCaptureVideoPreviewLayer?
    var photoOutput: AVCapturePhotoOutput?

    // MARK: - Lifecycle Methods
    override func viewDidLoad() {
        super.viewDidLoad()
        checkPermissions()
        setupCaptureSession()
        setupPreviewLayer()
        captureSession?.startRunning()
        print("CameraViewController: Capture session started.")
    }

    override func viewDidLayoutSubviews() {
        super.viewDidLayoutSubviews()
        videoPreviewLayer?.frame = previewView.bounds
    }

    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        // Restart the capture session when returning to CameraViewController
        if let captureSession = captureSession, !captureSession.isRunning {
            captureSession.startRunning()
            print("CameraViewController: Capture session restarted.")
        }

        // Reinitialize the preview layer if needed
        if let videoPreviewLayer = videoPreviewLayer, videoPreviewLayer.superlayer == nil {
            videoPreviewLayer.frame = previewView.bounds
            previewView.layer.addSublayer(videoPreviewLayer)
            print("CameraViewController: Preview layer re-added.")
        }
    }

    override func viewWillDisappear(_ animated: Bool) {
        super.viewWillDisappear(animated)
        // Stop the capture session when leaving CameraViewController
        if let captureSession = captureSession, captureSession.isRunning {
            captureSession.stopRunning()
            print("CameraViewController: Capture session stopped.")
        }
    }

    // MARK: - Camera Setup
    func checkPermissions() {
        switch AVCaptureDevice.authorizationStatus(for: .video) {
        case .authorized:
            // Permission granted; proceed with setup
            break
        case .notDetermined:
            // Request permission
            AVCaptureDevice.requestAccess(for: .video) { granted in
                if !granted {
                    DispatchQueue.main.async {
                        self.showPermissionAlert()
                    }
                }
            }
        default:
            // Permission denied; show alert
            DispatchQueue.main.async {
                self.showPermissionAlert()
            }
        }
    }

    func showPermissionAlert() {
        let alert = UIAlertController(title: "Camera Access Denied",
                                      message: "Please enable camera access in Settings",
                                      preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "Settings", style: .default) { _ in
            if let appSettings = URL(string: UIApplication.openSettingsURLString) {
                UIApplication.shared.open(appSettings, options: [:], completionHandler: nil)
            }
        })
        alert.addAction(UIAlertAction(title: "Cancel", style: .cancel))
        present(alert, animated: true)
    }

    func setupCaptureSession() {
        captureSession = AVCaptureSession()
        captureSession?.sessionPreset = .high

//        guard let camera = AVCaptureDevice.default(for: .video) else {
//            print("No camera available.")
//            return
//        }
        guard let camera = AVCaptureDevice.default(.builtInWideAngleCamera, for: .video, position: .front) else {
            print("No front camera available.")
            return
        }

        do {
            let input = try AVCaptureDeviceInput(device: camera)
            photoOutput = AVCapturePhotoOutput()

            if let captureSession = captureSession, let photoOutput = photoOutput {
                // Add Camera Input
                if captureSession.canAddInput(input) {
                    captureSession.addInput(input)
                }

                // Add Photo Output
                if captureSession.canAddOutput(photoOutput) {
                    captureSession.addOutput(photoOutput)
                }
            }
        } catch {
            print("Error setting up camera input: \(error)")
        }
    }

    func setupPreviewLayer() {
        guard let captureSession = captureSession else { return }
        videoPreviewLayer = AVCaptureVideoPreviewLayer(session: captureSession)
        videoPreviewLayer?.videoGravity = .resizeAspectFill
        videoPreviewLayer?.frame = previewView.bounds
        previewView.layer.addSublayer(videoPreviewLayer!)
    }

    // MARK: - IBAction Methods
    @IBAction func capturePhotoTapped(_ sender: UIButton) {
        let settings = AVCapturePhotoSettings()
        settings.flashMode = .off
        photoOutput?.capturePhoto(with: settings, delegate: self)
    }

    @IBAction func recordVideoTapped(_ sender: UIButton) {
        // Stop the capture session before transitioning
        captureSession?.stopRunning()
        print("CameraViewController: Capture session stopped before transitioning to VideoPredictionViewController.")

        let storyboard = UIStoryboard(name: "Main", bundle: nil)
        if let videoVC = storyboard.instantiateViewController(withIdentifier: "VideoPredictionViewController") as? VideoPredictionViewController {
            navigationController?.pushViewController(videoVC, animated: true)
        }
    }
}

// MARK: - Photo Capture Delegate
extension CameraViewController: AVCapturePhotoCaptureDelegate {
    func photoOutput(_ output: AVCapturePhotoOutput,
                     didFinishProcessingPhoto photo: AVCapturePhoto,
                     error: Error?) {
        guard let imageData = photo.fileDataRepresentation(),
              let image = UIImage(data: imageData) else { return }

        // Transition to PhotoPredictionViewController
        let storyboard = UIStoryboard(name: "Main", bundle: nil)
        if let photoVC = storyboard.instantiateViewController(withIdentifier: "PhotoPredictionViewController") as? PhotoPredictionViewController {
            photoVC.capturedImage = image
            navigationController?.pushViewController(photoVC, animated: true)
        }
    }
}



