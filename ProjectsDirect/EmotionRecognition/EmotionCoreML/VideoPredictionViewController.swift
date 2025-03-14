import UIKit
import AVFoundation
import Vision

class VideoPredictionViewController: UIViewController {

    // MARK: - IBOutlets
    @IBOutlet weak var previewView: UIView!
//    @IBOutlet weak var overlayView: UIView! // New overlay view for bounding boxes
    @IBOutlet weak var predictionLabel: UILabel!
    @IBOutlet weak var imageView: UIImageView! // For displaying normalized images

    // MARK: - Properties
    var captureSession: AVCaptureSession?
    var videoPreviewLayer: AVCaptureVideoPreviewLayer?
    var videoOutput: AVCaptureVideoDataOutput?
    var isProcessingVideo: Bool = true // Flag for frame processing
    private var lastFrameTime: TimeInterval = 0
    var isViewReady: Bool = false // Flag to ensure view is fully loaded

    // Reuse the Core ML model
    lazy var emotionModel: VNCoreMLModel? = {
        do {
            return try VNCoreMLModel(for: EmotionRecognition().model)
        } catch {
            print("Failed to load Core ML model: \(error)")
            return nil
        }
    }()

    override func viewDidLoad() {
        super.viewDidLoad()
        predictionLabel.text = "Prediction: [Processing...]"
        setupLiveVideoPreview()
        isViewReady = true // Set the flag to true
        setupLiveVideoPreview()
        imageView.frame = previewView.bounds
//        previewView.addSubview(imageView)

    }

    override func viewDidLayoutSubviews() {
        super.viewDidLayoutSubviews()

        // Match the preview layer to the preview view bounds
        videoPreviewLayer?.frame = previewView.bounds

    }

    override func viewWillDisappear(_ animated: Bool) {
        super.viewWillDisappear(animated)
        // Stop video processing when navigating back
        stopProcessing()
    }

    // MARK: - Video Setup
    func setupImageView(image: UIImage, view: UIView) -> UIImageView {
        let imageView = UIImageView(image: image)
        imageView.frame = view.bounds
        imageView.contentMode = .scaleAspectFit // or .scaleAspectFill depending on your needs
        imageView.clipsToBounds = true
        view.addSubview(imageView)
        return imageView
    }
    
    func setupLiveVideoPreview() {
            // Initialize the capture session
            captureSession = AVCaptureSession()
            captureSession?.sessionPreset = .high

            // Select the front camera
            guard let camera = AVCaptureDevice.default(.builtInWideAngleCamera, for: .video, position: .front) else {
                print("No front camera available.")
                return
            }

            do {
                // Create camera input
                let input = try AVCaptureDeviceInput(device: camera)
                videoOutput = AVCaptureVideoDataOutput()

                guard let captureSession = captureSession, let videoOutput = videoOutput else { return }

                // Add camera input to capture session
                if captureSession.canAddInput(input) {
                    captureSession.addInput(input)
                    print("Front camera input added.")
                } else {
                    print("Unable to add front camera input.")
                    return
                }

                // Configure and add video output
                videoOutput.alwaysDiscardsLateVideoFrames = true
                videoOutput.videoSettings = [kCVPixelBufferPixelFormatTypeKey as String: kCVPixelFormatType_32BGRA]

                if captureSession.canAddOutput(videoOutput) {
                    captureSession.addOutput(videoOutput)
                    videoOutput.setSampleBufferDelegate(self, queue: DispatchQueue(label: "video_frame_processing"))
                    print("Video output added.")
                } else {
                    print("Unable to add video output.")
                    return
                }

                // Set up the live preview
                setupPreviewLayer()

                // Start the capture session
                captureSession.startRunning()
                print("Capture session started.")

                // Configure video connection
                DispatchQueue.main.async {
                    self.configureVideoConnection()
                }

            } catch {
                print("Error setting up camera input: \(error)")
            }
        }

        // MARK: - Preview Layer Setup
        func setupPreviewLayer() {
            guard let captureSession = captureSession else { return }

            // Initialize the preview layer
            videoPreviewLayer = AVCaptureVideoPreviewLayer(session: captureSession)
            videoPreviewLayer?.videoGravity = .resizeAspectFill
            videoPreviewLayer?.frame = previewView.bounds

            // Add the preview layer to the view
            if let videoPreviewLayer = videoPreviewLayer {
                previewView.layer.addSublayer(videoPreviewLayer)
                print("Preview layer added.")
            }
        }

        // MARK: - Configure Video Connection
    func configureVideoConnection() {
        guard let connection = videoOutput?.connection(with: .video) else {
            print("Video output connection is nil.")
            return
        }

        // Set the video rotation angle based on the current device orientation
        if connection.isVideoRotationAngleSupported(90) {
            // Example: Check if a 90-degree rotation is supported
            connection.videoRotationAngle = angleForOrientation(UIDevice.current.orientation)
            print("Video rotation angle set to \(connection.videoRotationAngle) degrees.")
        }

        // Enable mirroring for the front camera
        connection.isVideoMirrored = false
        print("Mirroring enabled: \(connection.isVideoMirrored)")
    }

    func angleForOrientation(_ orientation: UIDeviceOrientation) -> CGFloat {
        switch orientation {
        case .portrait:
            return 0
        case .landscapeRight:
            return 90
        case .portraitUpsideDown:
            return 180
        case .landscapeLeft:
            return 270
        default:
            return 0
        }
    }

        // MARK: - Orientation Changes
        func observeOrientationChanges() {
            NotificationCenter.default.addObserver(
                self,
                selector: #selector(deviceOrientationDidChange),
                name: UIDevice.orientationDidChangeNotification,
                object: nil
            )
        }

        @objc func deviceOrientationDidChange(notification: Notification) {
            configureVideoConnection()
        }
    // MARK: - Processing Control
    func stopProcessing() {
        print("VideoPredictionViewController: Stopping processing.")
        isProcessingVideo = false

        DispatchQueue.main.async {
            self.videoOutput?.setSampleBufferDelegate(nil, queue: nil)
            self.captureSession?.stopRunning()
            self.videoPreviewLayer?.removeFromSuperlayer()
            self.captureSession = nil
            self.videoOutput = nil
        }
    }

    // MARK: - IBAction Methods
    @IBAction func backButtonTapped(_ sender: UIButton) {
        stopProcessing()
        navigationController?.popViewController(animated: true)
    }

    func detectFace(in image: UIImage) {
        guard let cgImage = image.cgImage else {
            DispatchQueue.main.async {
                self.predictionLabel.text = "Failed to create CGImage for face detection."
            }
            return
        }

        let faceDetectionRequest = VNDetectFaceRectanglesRequest { [weak self] (request, error) in
            guard let self = self else { return }

            if let results = request.results as? [VNFaceObservation] {
                if results.isEmpty {
                    DispatchQueue.main.async {
                        self.predictionLabel.text = "No face detected."
                        self.clearBoundingBoxes()
                        self.imageView.isHidden = true
                    }
                } else {
                    // Log the number of detected faces
                    print("Faces detected: \(results.count)")

                    // Update bounding boxes
                    DispatchQueue.main.async {
                        self.updateBoundingBoxes(with: results)
                    }

                    // Perform further processing for each detected face
                    for faceObservation in results {
                        if let faceImage = self.cropFace(from: image, using: faceObservation) {
                            let resizedFaceImage = self.resizeImage(faceImage, to: CGSize(width: 224, height: 224))
                            self.performEmotionRecognition(on: resizedFaceImage)
                        }
                    }
                }
            } else {
                DispatchQueue.main.async {
                    self.predictionLabel.text = "Error detecting face: \(error)?.localizedDescription ?? \"Unknown error\")"
                    self.clearBoundingBoxes()
                    self.imageView.isHidden = true
                }
            }
        }

        // Set the orientation to match the image's orientation (.up)
        let handler = VNImageRequestHandler(cgImage: cgImage, orientation: .up, options: [:])
        DispatchQueue.global(qos: .userInitiated).async {
            do {
                try handler.perform([faceDetectionRequest])
            } catch {
                DispatchQueue.main.async {
                    self.predictionLabel.text = "Failed to detect face."
                    self.imageView.isHidden = true
                }
                print("Error performing face detection: \(error.localizedDescription)")
            }
        }
    }

    func cropFace(from image: UIImage, using observation: VNFaceObservation) -> UIImage? {
        guard let cgImage = image.cgImage else { return nil }

        let boundingBox = observation.boundingBox
        let imageSize = CGSize(width: cgImage.width, height: cgImage.height)

        // Convert bounding box to image coordinates
        var faceRect = CGRect(
            x: boundingBox.origin.x * imageSize.width,
            y: (1 - boundingBox.origin.y - boundingBox.size.height) * imageSize.height,
            width: boundingBox.size.width * imageSize.width,
            height: boundingBox.size.height * imageSize.height
        )

        // Ensure the faceRect is within the image bounds
        faceRect = faceRect.intersection(CGRect(origin: .zero, size: imageSize))

        // Crop the image
        guard let croppedCgImage = cgImage.cropping(to: faceRect) else {
            return nil
        }

        let croppedFaceImage = UIImage(cgImage: croppedCgImage)

        // Return the cropped face image without drawing a bounding box here
        return croppedFaceImage
    }

    func resizeImage(_ image: UIImage, to size: CGSize) -> UIImage {
        UIGraphicsBeginImageContextWithOptions(size, false, image.scale)
        image.draw(in: CGRect(origin: .zero, size: size))
        let resizedImage = UIGraphicsGetImageFromCurrentImageContext()!
        UIGraphicsEndImageContext()
        return resizedImage
    }

    func performEmotionRecognition(on faceImage: UIImage) {
        guard let model = emotionModel else {
            DispatchQueue.main.async {
                self.predictionLabel.text = "Failed to load emotion model."
            }
            return
        }

        // Create a Vision request
        let request = VNCoreMLRequest(model: model) { [weak self] request, error in
            guard let self = self else { return }

            if let results = request.results as? [VNCoreMLFeatureValueObservation] {
                // Process `emotion_scores`
                if let emotionScoresArray = results.first(where: { $0.featureName == "emotion_scores" })?.featureValue.multiArrayValue {
                    let emotionScores = (0..<emotionScoresArray.count).map { emotionScoresArray[$0].floatValue }
                    let emotionLabels = ["Neutral", "Happiness", "Sadness", "Surprise", "Fear", "Disgust", "Anger"]
                    if let maxIndex = emotionScores.firstIndex(of: emotionScores.max() ?? 0) {
                        let predictedEmotion = emotionLabels[maxIndex]
                        // let confidence = emotionScores[maxIndex] * 100

                        // Process `valence`
                        let valence = results.first(where: { $0.featureName == "valence" })?.featureValue.multiArrayValue?[0].floatValue ?? 0.0

                        // Process `arousal`
                        let arousal = results.first(where: { $0.featureName == "arousal" })?.featureValue.multiArrayValue?[0].floatValue ?? 0.0

                        // Update the UI
                        DispatchQueue.main.async {
                            self.predictionLabel.text = """
                            Emotion: \(predictedEmotion)
                            Valence: \(String(format: "%.2f", valence))
                            Arousal: \(String(format: "%.2f", arousal))
                            """
                            self.imageView.image = faceImage
                            self.imageView.isHidden = true
                        }
                    }
                }
            } else {
                DispatchQueue.main.async {
                    self.predictionLabel.text = "Unable to classify emotion."
                    self.imageView.isHidden = true
                }
            }
        }

        // Prepare the image for prediction
        guard let cgImage = faceImage.cgImage else {
            DispatchQueue.main.async {
                self.predictionLabel.text = "Failed to prepare image."
                self.imageView.isHidden = true
            }
            return
        }
        let handler = VNImageRequestHandler(cgImage: cgImage, options: [:])

        // Perform the request
        DispatchQueue.global(qos: .userInitiated).async {
            do {
                try handler.perform([request])
            } catch {
                DispatchQueue.main.async {
                    self.predictionLabel.text = "Failed to perform emotion recognition."
                    self.imageView.isHidden = true
                }
                print("Error performing classification: \(error.localizedDescription)")
            }
        }
    }

    // MARK: - Bounding Box Management
    func updateBoundingBoxes(with observations: [VNFaceObservation]) {
        // Clear existing bounding boxes
        clearBoundingBoxes()

        // Draw bounding boxes for each detected face
        for observation in observations {
            drawBoundingBox(on: previewView, boundingBox: observation.boundingBox)
        }
    }

    func clearBoundingBoxes() {
        DispatchQueue.main.async {
            self.previewView.layer.sublayers?.removeAll(where: { $0 is CAShapeLayer })
        }
    }


    func mapBoundingBoxToView(boundingBox: CGRect, in view: UIView) -> CGRect {
        return CGRect(
            x: (1 - boundingBox.origin.x - boundingBox.width) * view.bounds.width,
            y: (1 - boundingBox.origin.y - boundingBox.height) * view.bounds.height, // Flip Y-axis, // No Y-axis flip
            width: boundingBox.width * view.bounds.width,
            height: boundingBox.height * view.bounds.height
        )
    }
    
    func drawBoundingBox(on view: UIView, boundingBox: CGRect) {
        // Map the bounding box to the view's size
        let absoluteBoundingBox = mapBoundingBoxToView(boundingBox: boundingBox, in: view)

        // Create a shape layer
        let shapeLayer = CAShapeLayer()
        shapeLayer.strokeColor = UIColor.red.cgColor
        shapeLayer.lineWidth = 2.0
        shapeLayer.fillColor = UIColor.clear.cgColor
        shapeLayer.path = UIBezierPath(rect: absoluteBoundingBox).cgPath

        // Add the shape layer to the view
        DispatchQueue.main.async {
            view.layer.addSublayer(shapeLayer)
        }

        // Debugging Logs
        print("Absolute Bounding Box: \(absoluteBoundingBox)")
    }

}

// MARK: - AVCaptureVideoDataOutputSampleBufferDelegate
extension VideoPredictionViewController: AVCaptureVideoDataOutputSampleBufferDelegate {
    func captureOutput(_ output: AVCaptureOutput,
                       didOutput sampleBuffer: CMSampleBuffer,
                       from connection: AVCaptureConnection) {
        guard isProcessingVideo else { return }

        let currentTime = CACurrentMediaTime()
        guard currentTime - lastFrameTime > 0.5 else { return } // Process one frame every 0.5 seconds
        lastFrameTime = currentTime

        guard let pixelBuffer = CMSampleBufferGetImageBuffer(sampleBuffer) else {
            print("Failed to get pixel buffer.")
            return
        }

        // Convert the pixel buffer to UIImage for processing
        let ciImage = CIImage(cvPixelBuffer: pixelBuffer)
        let context = CIContext()
        guard let cgImage = context.createCGImage(ciImage, from: ciImage.extent) else {
            print("Failed to create CGImage from pixel buffer.")
            return
        }

        // Dynamically determine the correct orientation
        let imageOrientation: UIImage.Orientation
        if connection.isVideoMirrored {
            switch UIDevice.current.orientation {
            case .portrait:
                imageOrientation = .leftMirrored
            case .landscapeRight:
                imageOrientation = .downMirrored
            case .portraitUpsideDown:
                imageOrientation = .rightMirrored
            case .landscapeLeft:
                imageOrientation = .upMirrored
            default:
                imageOrientation = .leftMirrored
            }
        } else {
            switch UIDevice.current.orientation {
            case .portrait:
                imageOrientation = .right
            case .landscapeRight:
                imageOrientation = .up
            case .portraitUpsideDown:
                imageOrientation = .left
            case .landscapeLeft:
                imageOrientation = .down
            default:
                imageOrientation = .right
            }
        }

        // Create UIImage with the correct orientation
        let originalImage = UIImage(cgImage: cgImage, scale: 1.0, orientation: imageOrientation)
        let normalizedImage = originalImage.normalizedImage()

        DispatchQueue.main.async {
            if let imageView = self.imageView {
                imageView.image = normalizedImage // Display the normalized image
            } else {
                print("Error: imageView is nil.")
            }
        }

        // Perform face detection on the full-resolution image
        detectFace(in: normalizedImage)
    }

}
