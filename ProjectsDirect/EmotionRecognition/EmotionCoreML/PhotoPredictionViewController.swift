import UIKit
import CoreML
import Vision
import ImageIO

class PhotoPredictionViewController: UIViewController {

    // MARK: - IBOutlets
    @IBOutlet weak var imageView: UIImageView!
    @IBOutlet weak var predictionLabel: UILabel!

    // MARK: - Properties
    var capturedImage: UIImage?  // Image passed from CameraViewController

    // MARK: - Lifecycle Methods
    override func viewDidLoad() {
        super.viewDidLoad()
        predictionLabel.text = "Prediction: [Processing...]"

        // Display the captured image and process it
        if let image = capturedImage {
            processImage(image)
        } else {
            predictionLabel.text = "No image captured."
        }
    }

    override func viewWillDisappear(_ animated: Bool) {
        super.viewWillDisappear(animated)
        // Reset the imageView and predictionLabel for the next capture
        imageView.image = nil
        predictionLabel.text = "Prediction: [Processing...]"
    }

    // MARK: - Image Processing
    func processImage(_ image: UIImage) {
        // Normalize the image orientation
        let normalizedImage = image.normalizedImage()
        DispatchQueue.main.async {
            self.imageView.image = normalizedImage  // Display the normalized image
        }

        guard let cgImage = normalizedImage.cgImage else {
            predictionLabel.text = "Unable to create CGImage."
            return
        }

        // Create a face detection request
        let faceDetectionRequest = VNDetectFaceRectanglesRequest { [weak self] (request, error) in
            guard let self = self else { return }

            if let results = request.results as? [VNFaceObservation], let faceObservation = results.first {
                // Crop the face region
                if let faceImage = self.cropFace(from: normalizedImage, using: faceObservation) {
                    // Resize face image to 224x224
                    let resizedFaceImage = self.resizeImage(faceImage, to: CGSize(width: 224, height: 224))
                    // Proceed with emotion recognition
                    self.performEmotionRecognition(on: resizedFaceImage)
                } else {
                    DispatchQueue.main.async {
                        self.predictionLabel.text = "Failed to crop face."
                    }
                }
            } else {
                DispatchQueue.main.async {
                    self.predictionLabel.text = "No face detected."
                }
            }
        }

        // Perform the face detection request
        let handler = VNImageRequestHandler(cgImage: cgImage, orientation: .up, options: [:])  // Using .up since we've normalized the image
        DispatchQueue.global(qos: .userInitiated).async {
            do {
                try handler.perform([faceDetectionRequest])
            } catch {
                DispatchQueue.main.async {
                    self.predictionLabel.text = "Failed to detect face."
                }
                print("Error detecting face: \(error.localizedDescription)")
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

        // Create a UIImage from the cropped CGImage
        let croppedFaceImage = UIImage(cgImage: croppedCgImage)

        // Optional: Draw bounding box on the image for debugging
        // Uncomment the following lines to see the bounding box

        let annotatedImage = drawBoundingBox(on: image, boundingBox: faceRect)
        DispatchQueue.main.async {
            self.imageView.image = annotatedImage
        }


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
        // Load the Core ML model
        guard let model = try? VNCoreMLModel(for: EmotionRecognition2().model) else {
            fatalError("Failed to load model")
        }

        // Create a Vision request
        let request = VNCoreMLRequest(model: model) { [weak self] request, error in
            guard let self = self else { return }

            if let results = request.results as? [VNCoreMLFeatureValueObservation] {
                // Process `emotion_scores`
                if let scoresArray = results.first(where: { $0.featureName == "age" })?.featureValue.multiArrayValue {
                    let scores = (0..<scoresArray.count).map { scoresArray[$0].floatValue }
                    
                    // Process Age (0-100)
                    let ageScores = scores[0..<100]
                    let predictedAge = ageScores.firstIndex(of: ageScores.max() ?? 0) ?? 0

                    // Process Gender (101-102)
                    let genderScores = scores[0..<1]
                    let genderLabels = ["Male", "Female"]
                    let predictedGenderIndex = genderScores.firstIndex(of: genderScores.max() ?? 0) ?? 0
                    let predictedGender = genderLabels[predictedGenderIndex]

                    // Process Ethnicity (103-108)
                    let ethnicityScores = scores[0..<6]
                    let ethnicityLabels = ["Ethnicity1", "Ethnicity2", "Ethnicity3", "Ethnicity4", "Ethnicity5", "Ethnicity6"]
                    let predictedEthnicityIndex = ethnicityScores.firstIndex(of: ethnicityScores.max() ?? 0) ?? 0
                    let predictedEthnicity = ethnicityLabels[predictedEthnicityIndex]

                    // Update the UI
                    DispatchQueue.main.async {
                        self.predictionLabel.text = """
                        Age: \(predictedAge)
                        Gender: \(predictedGender)
                        Ethnicity: \(predictedEthnicity)
                        """
                    }
                }
//                if let emotionScoresArray = results.first(where: { $0.featureName == "emotion_scores" })?.featureValue.multiArrayValue {
//                    let emotionScores = (0..<emotionScoresArray.count).map { emotionScoresArray[$0].floatValue }
//                    let emotionLabels = ["Neutral", "Happiness", "Sadness", "Surprise", "Fear", "Disgust", "Anger"]
//                    if let maxIndex = emotionScores.firstIndex(of: emotionScores.max() ?? 0) {
//                        let predictedEmotion = emotionLabels[maxIndex]
////                        let confidence = emotionScores[maxIndex] * 100
//
//                        // Process `valence`
//                        let valence = results.first(where: { $0.featureName == "valence" })?.featureValue.multiArrayValue?[0].floatValue ?? 0.0
//                        
//                        // Process `arousal`
//                        let arousal = results.first(where: { $0.featureName == "arousal" })?.featureValue.multiArrayValue?[0].floatValue ?? 0.0
//                        
//                        // Update the UI
//                        DispatchQueue.main.async {
//                            self.predictionLabel.text = """
//                            Emotion: \(predictedEmotion)
//                            Valence: \(String(format: "%.2f", valence))
//                            Arousal: \(String(format: "%.2f", arousal))
//                            """
////                            self.imageView.image = faceImage
//                        }
//                    }
//                }
            } else {
                DispatchQueue.main.async {
                    self.predictionLabel.text = "Unable to classify emotion."
                }
            }
        }

        // Prepare the image for prediction
        guard let cgImage = faceImage.cgImage else {
            predictionLabel.text = "Failed to prepare image."
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
                }
                print("Error performing classification: \(error.localizedDescription)")
            }
        }
    }




    // MARK: - Helper Methods
    func drawBoundingBox(on image: UIImage, boundingBox: CGRect) -> UIImage {
        UIGraphicsBeginImageContextWithOptions(image.size, false, image.scale)
        let context = UIGraphicsGetCurrentContext()!
        image.draw(at: .zero)

        context.setStrokeColor(UIColor.red.cgColor)
        context.setLineWidth(2.0)
        context.stroke(boundingBox)
        
        

        let annotatedImage = UIGraphicsGetImageFromCurrentImageContext()!
        UIGraphicsEndImageContext()
        return annotatedImage
    }
}

//// MARK: - UIImage Extensions
//extension UIImage {
//    // Normalize the image orientation to .up
//    func normalizedImage() -> UIImage {
//        if imageOrientation == .up {
//            return self
//        }
//        UIGraphicsBeginImageContextWithOptions(size, false, scale)
//        draw(in: CGRect(origin: .zero, size: size))
//        let normalizedImage = UIGraphicsGetImageFromCurrentImageContext()!
//        UIGraphicsEndImageContext()
//        return normalizedImage
//    }
//
//    // Convert UIImageOrientation to CGImagePropertyOrientation
//    var cgImagePropertyOrientation: CGImagePropertyOrientation {
//        return .up  // Since we've normalized the image, it's always .up
//    }
//}

//
//  PhotoPredictionViewController.swift
//  Project Emotion Recognition
//
//  Created by Michael Guel on 11/20/24.
//

//import UIKit
//import Foundation
//
//class PhotoPredictionViewController: UIViewController {
//
//    @IBOutlet weak var imageView: UIImageView!
//    @IBOutlet weak var predictionLabel: UILabel!
//
//    var capturedImage: UIImage? // Placeholder to receive the photo
//
//    override func viewDidLoad() {
//        super.viewDidLoad()
//        imageView.image = capturedImage
//        predictionLabel.text = "Prediction: [Placeholder]"
//    }
//}

//import UIKit
//
//class PhotoPredictionViewController: UIViewController {
//
//    @IBOutlet weak var imageView: UIImageView!
//    @IBOutlet weak var predictionLabel: UILabel!
//
//    var capturedImage: UIImage?  // Image passed from CameraViewController
//    var model: TorchModule?      // PyTorch Mobile model
//
//    override func viewDidLoad() {
//        super.viewDidLoad()
//        predictionLabel.text = "Prediction: [Processing...]"
//
//        // Display the captured image
//        if let image = capturedImage {
//            imageView.image = image
//        }
//
//        // Load the TorchScript model
//        loadTorchScriptModel()
//
//        // Process the image for emotion prediction
//        if let image = capturedImage {
//            processImage(image)
//        }
//    }
//
//    func loadTorchScriptModel() {
//        // Load the .pt file from the app bundle
//        if let filePath = Bundle.main.path(forResource: "hsemotion_model", ofType: "pt") {
//            model = TorchModule(fileAtPath: filePath)
//        } else {
//            print("Failed to load TorchScript model.")
//        }
//    }
//
//    func processImage(_ image: UIImage) {
//        guard let model = model else {
//            predictionLabel.text = "Model not loaded."
//            return
//        }
//
//        // Preprocess the image (resize, normalize)
//        guard let tensor = preprocessImage(image) else {
//            predictionLabel.text = "Failed to preprocess image."
//            return
//        }
//
//        // Perform inference
//        let output = model.forward(tensor)
//        let emotion = interpretOutput(output)
//
//        // Update the UI with the prediction
//        DispatchQueue.main.async {
//            self.predictionLabel.text = "Emotion: \(emotion)"
//        }
//    }
//
//    func preprocessImage(_ image: UIImage) -> Tensor? {
//        // Resize the image to 224x224
//        guard let resizedImage = resizeImage(image, to: CGSize(width: 224, height: 224)),
//              let pixelBuffer = resizedImage.pixelBuffer(width: 224, height: 224) else {
//            return nil
//        }
//
//        // Normalize the image
//        let mean: [Float] = [0.485, 0.456, 0.406]
//        let std: [Float] = [0.229, 0.224, 0.225]
//
//        // Convert the image to a PyTorch Tensor
//        return Tensor(fromPixelBuffer: pixelBuffer, mean: mean, std: std)
//    }
//
//    func resizeImage(_ image: UIImage, to size: CGSize) -> UIImage? {
//        UIGraphicsBeginImageContext(size)
//        image.draw(in: CGRect(origin: .zero, size: size))
//        let resizedImage = UIGraphicsGetImageFromCurrentImageContext()
//        UIGraphicsEndImageContext()
//        return resizedImage
//    }
//
//    func interpretOutput(_ output: Tensor) -> String {
//        // Extract scores and determine the highest-scoring emotion
//        guard let scores = output.data as? [Float] else { return "Unknown" }
//
//        // Replace with your model's emotion labels
//        let emotions = ["Neutral", "Happy", "Sad", "Angry", "Fearful", "Disgusted", "Surprised"]
//        if let maxIndex = scores.firstIndex(of: scores.max() ?? 0) {
//            return emotions[maxIndex]
//        }
//        return "Unknown"
//    }
//}

//import UIKit
//import CoreML
//import Vision
//
//class PhotoPredictionViewController: UIViewController {
//
//    // MARK: - IBOutlets
//    @IBOutlet weak var imageView: UIImageView!
//    @IBOutlet weak var predictionLabel: UILabel!
//
//    // MARK: - Properties
//    var capturedImage: UIImage?  // Image passed from CameraViewController
//
//    // MARK: - Lifecycle Methods
//    override func viewDidLoad() {
//        super.viewDidLoad()
//        predictionLabel.text = "Prediction: [Processing...]"
//
//        // Display the captured image and process it
//        if let image = capturedImage {
//            imageView.image = image
//            processImage(image)
//        } else {
//            predictionLabel.text = "No image captured."
//        }
//    }
//
//    // MARK: - Image Processing
//    func processImage(_ image: UIImage) {
//        // Correct the image orientation
//        guard let correctedImage = image.fixOrientation() else {
//            predictionLabel.text = "Failed to fix image orientation."
//            return
//        }
//
//        // Create a CIImage from the corrected UIImage
//        guard let ciImage = CIImage(image: correctedImage) else {
//            predictionLabel.text = "Unable to create CIImage."
//            return
//        }
//
//        // Load the Core ML model
//        guard let model = try? VNCoreMLModel(for: EmotionRecognition().model) else {
//            predictionLabel.text = "Failed to load ML model."
//            return
//        }
//
//        // Create a Vision request
//        let request = VNCoreMLRequest(model: model) { [weak self] request, error in
//            guard let self = self else { return }
//
//            if let results = request.results as? [VNClassificationObservation],
//               let topResult = results.first {
//                // Update the UI with the prediction
//                DispatchQueue.main.async {
//                    let confidence = Int(topResult.confidence * 100)
//                    self.predictionLabel.text = "Emotion: \(topResult.identifier) (\(confidence)%)"
//                }
//            } else {
//                DispatchQueue.main.async {
//                    self.predictionLabel.text = "Unable to classify image."
//                }
//            }
//        }
//
//        // Perform the request
//        let handler = VNImageRequestHandler(ciImage: ciImage, options: [:])
//        DispatchQueue.global(qos: .userInitiated).async {
//            do {
//                try handler.perform([request])
//            } catch {
//                DispatchQueue.main.async {
//                    self.predictionLabel.text = "Failed to perform classification."
//                }
//                print("Error performing classification: \(error.localizedDescription)")
//            }
//        }
//    }
//}
//
//// MARK: - UIImage Extension
//extension UIImage {
//    func fixOrientation() -> UIImage? {
//        if imageOrientation == .up {
//            return self
//        }
//        UIGraphicsBeginImageContextWithOptions(size, false, scale)
//        draw(in: CGRect(origin: .zero, size: size))
//        let normalizedImage = UIGraphicsGetImageFromCurrentImageContext()
//        UIGraphicsEndImageContext()
//        return normalizedImage
//    }
//}

//import UIKit
//import CoreML
//import Vision
//
//class PhotoPredictionViewController: UIViewController {
//
//    // MARK: - IBOutlets
//    @IBOutlet weak var imageView: UIImageView!
//    @IBOutlet weak var predictionLabel: UILabel!
//
//    // MARK: - Properties
//    var capturedImage: UIImage?  // Image passed from CameraViewController
//
//    // MARK: - Lifecycle Methods
//    override func viewDidLoad() {
//        super.viewDidLoad()
//        predictionLabel.text = "Prediction: [Processing...]"
//
//        // Display the captured image and process it
//        if let image = capturedImage {
//            imageView.image = image
//            processImage(image)
//        } else {
//            predictionLabel.text = "No image captured."
//        }
//    }
//
//    // MARK: - Image Processing
//    func processImage(_ image: UIImage) {
//        guard let cgImage = image.cgImage else {
//            predictionLabel.text = "Unable to create CGImage."
//            return
//        }
//
//        // Create a face detection request
//        let faceDetectionRequest = VNDetectFaceRectanglesRequest { [weak self] (request, error) in
//            guard let self = self else { return }
//
//            if let results = request.results as? [VNFaceObservation], let faceObservation = results.first {
//                // Crop the face region
//                if let faceImage = self.cropFace(from: image, using: faceObservation) {
//                    // Resize face image to 224x224
//                    let resizedFaceImage = self.resizeImage(faceImage, to: CGSize(width: 224, height: 224))
//                    // Proceed with emotion recognition
//                    self.performEmotionRecognition(on: resizedFaceImage)
//                } else {
//                    DispatchQueue.main.async {
//                        self.predictionLabel.text = "Failed to crop face."
//                    }
//                }
//            } else {
//                DispatchQueue.main.async {
//                    self.predictionLabel.text = "No face detected."
//                }
//            }
//        }
//
//        // Perform the face detection request
//        let handler = VNImageRequestHandler(cgImage: cgImage, orientation: image.cgImagePropertyOrientation, options: [:])
//        DispatchQueue.global(qos: .userInitiated).async {
//            do {
//                try handler.perform([faceDetectionRequest])
//            } catch {
//                DispatchQueue.main.async {
//                    self.predictionLabel.text = "Failed to detect face."
//                }
//                print("Error detecting face: \(error.localizedDescription)")
//            }
//        }
//    }
//
//    func cropFace(from image: UIImage, using observation: VNFaceObservation) -> UIImage? {
//        // Get the bounding box
//        let boundingBox = observation.boundingBox
//
//        // Convert the bounding box to image coordinates
//        let faceRect = VNImageRectForNormalizedRect(boundingBox, Int(image.size.width), Int(image.size.height))
//
//        // Adjust for image orientation
//        guard let cgImage = image.cgImage?.cropping(to: faceRect) else {
//            return nil
//        }
//
//        let renderer = UIGraphicsImageRenderer(size: image.size)
//        let annotatedImage = renderer.image { ctx in
//            image.draw(at: .zero)
//            ctx.cgContext.setStrokeColor(UIColor.red.cgColor)
//            ctx.cgContext.setLineWidth(2)
//            ctx.cgContext.stroke(faceRect)
//        }
//
//        DispatchQueue.main.async {
//            self.imageView.image = annotatedImage
//        }
//
//        return UIImage(cgImage: cgImage, scale: image.scale, orientation: image.imageOrientation)
//    }
//
//    func resizeImage(_ image: UIImage, to size: CGSize) -> UIImage {
//        UIGraphicsBeginImageContextWithOptions(size, false, image.scale)
//        image.draw(in: CGRect(origin: .zero, size: size))
//        let resizedImage = UIGraphicsGetImageFromCurrentImageContext()
//        UIGraphicsEndImageContext()
//        return resizedImage!
//    }
//
//    func performEmotionRecognition(on faceImage: UIImage) {
//        guard let ciImage = CIImage(image: faceImage) else {
//            predictionLabel.text = "Unable to create CIImage from face image."
//            return
//        }
//
//        // Load the Core ML model
//        guard let model = try? VNCoreMLModel(for: EmotionRecognition().model) else {
//            predictionLabel.text = "Failed to load ML model."
//            return
//        }
//
//        // Create a Vision request for emotion recognition
//        let emotionRecognitionRequest = VNCoreMLRequest(model: model) { [weak self] request, error in
//            guard let self = self else { return }
//
//            if let results = request.results as? [VNClassificationObservation],
//               let topResult = results.first {
//                DispatchQueue.main.async {
//                    let confidence = Int(topResult.confidence * 100)
//                    self.predictionLabel.text = "Emotion: \(topResult.identifier) (\(confidence)%)"
//                    self.imageView.image = faceImage // Display the resized face image
//                }
//            } else {
//                DispatchQueue.main.async {
//                    self.predictionLabel.text = "Unable to classify emotion."
//                }
//            }
//        }
//
//        // Perform the emotion recognition request
//        let handler = VNImageRequestHandler(ciImage: ciImage, options: [:])
//        DispatchQueue.global(qos: .userInitiated).async {
//            do {
//                try handler.perform([emotionRecognitionRequest])
//            } catch {
//                DispatchQueue.main.async {
//                    self.predictionLabel.text = "Failed to perform emotion recognition."
//                }
//                print("Error performing emotion recognition: \(error.localizedDescription)")
//            }
//        }
//    }
//}
//
//// MARK: - UIImage Orientation Extension
//import ImageIO
//
//extension UIImage {
//    var cgImagePropertyOrientation: CGImagePropertyOrientation {
//        switch imageOrientation {
//        case .up: return .up
//        case .down: return .down
//        case .left: return .left
//        case .right: return .right
//        case .upMirrored: return .upMirrored
//        case .downMirrored: return .downMirrored
//        case .leftMirrored: return .leftMirrored
//        case .rightMirrored: return .rightMirrored
//        @unknown default:
//            return .up
//        }
//    }
//}



//    func performEmotionRecognition(on faceImage: UIImage) {
//        guard let ciImage = CIImage(image: faceImage) else {
//            predictionLabel.text = "Unable to create CIImage from face image."
//            return
//        }
//
//        // Load the Core ML model
//        guard let model = try? VNCoreMLModel(for: EmotionRecognition().model) else {
//            predictionLabel.text = "Failed to load ML model."
//            return
//        }
//
//        // Create a Vision request for emotion recognition
//        let emotionRecognitionRequest = VNCoreMLRequest(model: model) { [weak self] request, error in
//            guard let self = self else { return }
//
//            if let results = request.results as? [VNClassificationObservation],
//               let topResult = results.first {
//                DispatchQueue.main.async {
//                    let confidence = Int(topResult.confidence * 100)
//                    self.predictionLabel.text = "Emotion: \(topResult.identifier) (\(confidence)%)"
//                    self.imageView.image = faceImage  // Display the resized face image
//                }
//            } else {
//                DispatchQueue.main.async {
//                    self.predictionLabel.text = "Unable to classify emotion."
//                }
//            }
//        }
//
//        // Perform the emotion recognition request
//        let handler = VNImageRequestHandler(ciImage: ciImage, options: [:])
//        DispatchQueue.global(qos: .userInitiated).async {
//            do {
//                try handler.perform([emotionRecognitionRequest])
//            } catch {
//                DispatchQueue.main.async {
//                    self.predictionLabel.text = "Failed to perform emotion recognition."
//                }
//                print("Error performing emotion recognition: \(error.localizedDescription)")
//            }
//        }
//    }
