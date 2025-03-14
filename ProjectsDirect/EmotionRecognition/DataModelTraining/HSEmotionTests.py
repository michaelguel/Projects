from hsemotion.facial_emotions import HSEmotionRecognizer
import cv2
import mediapipe as mp

# Initialize Mediapipe Face Detection
mp_face_detection = mp.solutions.face_detection

# Initialize the emotion recognizer
recognizer = HSEmotionRecognizer(model_name='enet_b2_8', device='cuda')

# Load a sample image
image_path = "/Users/Owner/Downloads/IMG_8618.jpg"  # Replace with your actual image path
image = cv2.imread(image_path)

if image is None:
    print(f"Failed to load image at {image_path}")
else:
    # Convert BGR image to RGB for Mediapipe
    rgb_image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)

    # Perform face detection
    with mp_face_detection.FaceDetection(min_detection_confidence=0.5) as face_detection:
        results = face_detection.process(rgb_image)

        if results.detections:
            for detection in results.detections:
                # Extract bounding box
                bboxC = detection.location_data.relative_bounding_box
                ih, iw, _ = image.shape
                x, y, w, h = (
                    int(bboxC.xmin * iw),
                    int(bboxC.ymin * ih),
                    int(bboxC.width * iw),
                    int(bboxC.height * ih),
                )

                # Ensure coordinates are within image bounds
                x = max(0, x)
                y = max(0, y)
                w = min(iw - x, w)
                h = min(ih - y, h)

                # Crop face from the image
                cropped_face = image[y : y + h, x : x + w]

                # Predict emotions
                emotion, scores = recognizer.predict_emotions(cropped_face)
                print("Predicted Emotion:", emotion)
                print("Scores:", scores)

                # Draw bounding box and label on the image
                cv2.rectangle(image, (x, y), (x + w, y + h), (255, 0, 0), 2)
                cv2.putText(
                    image,
                    emotion,
                    (x, y - 10),
                    cv2.FONT_HERSHEY_SIMPLEX,
                    1.5,  # Increased font size
                    (255, 0, 0),  # Blue text
                    3,  # Increased thickness
                    cv2.LINE_AA,
                )

        else:
            print("No faces detected.")

    # Resize the image for better visibility
    scale_percent = 150  # Scale the image by 150%
    width = int(image.shape[1] * scale_percent / 100)
    height = int(image.shape[0] * scale_percent / 100)
    dim = (width, height)
    image = cv2.resize(image, dim, interpolation=cv2.INTER_LINEAR)

    # Create a named window with adjustable size
    cv2.namedWindow("Face Detection with Emotion Recognition", cv2.WINDOW_NORMAL)
    cv2.resizeWindow("Face Detection with Emotion Recognition", 675, 900)

    # Display the image with detections
    cv2.imshow("Face Detection with Emotion Recognition", image)

    # Wait for a key press to close the window
    while True:
        key = cv2.waitKey(1) & 0xFF
        if key == ord('q') or cv2.getWindowProperty("Face Detection with Emotion Recognition", cv2.WND_PROP_VISIBLE) < 1:
            break

    cv2.destroyAllWindows()