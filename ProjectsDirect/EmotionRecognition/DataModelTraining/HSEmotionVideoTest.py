from hsemotion.facial_emotions import HSEmotionRecognizer
import cv2
import mediapipe as mp

# Initialize Mediapipe Face Detection
mp_face_detection = mp.solutions.face_detection

# Initialize the emotion recognizer
recognizer = HSEmotionRecognizer(model_name='enet_b2_8', device='cuda')

# Open webcam video stream
video_capture = cv2.VideoCapture(0)  # 0 refers to the default webcam

# Check if the webcam is opened successfully
if not video_capture.isOpened():
    print("Error: Could not access the webcam.")
else:
    print("Webcam accessed successfully. Press 'q' to exit.")

# Process video stream frame by frame
with mp_face_detection.FaceDetection(min_detection_confidence=0.5) as face_detection:
    while True:
        # Capture a single frame
        ret, frame = video_capture.read()

        # Break the loop if the frame is not captured successfully
        if not ret:
            print("Failed to grab frame. Exiting...")
            break

        # Convert BGR frame to RGB for Mediapipe
        rgb_frame = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)

        # Perform face detection
        results = face_detection.process(rgb_frame)

        if results.detections:
            for detection in results.detections:
                # Extract bounding box
                bboxC = detection.location_data.relative_bounding_box
                ih, iw, _ = frame.shape
                x, y, w, h = (
                    int(bboxC.xmin * iw),
                    int(bboxC.ymin * ih),
                    int(bboxC.width * iw),
                    int(bboxC.height * ih),
                )

                # Ensure coordinates are within frame bounds
                x = max(0, x)
                y = max(0, y)
                w = min(iw - x, w)
                h = min(ih - y, h)

                # Crop face from the frame
                cropped_face = frame[y : y + h, x : x + w]

                # Predict emotions
                try:
                    emotion, scores = recognizer.predict_emotions(cropped_face)
                except Exception as e:
                    print(f"Error during emotion prediction: {e}")
                    continue

                # Draw bounding box and label on the frame
                cv2.rectangle(frame, (x, y), (x + w, y + h), (255, 0, 0), 2)
                cv2.putText(
                    frame,
                    emotion,
                    (x, y - 10),
                    cv2.FONT_HERSHEY_SIMPLEX,
                    1.0,  # Font size
                    (255, 0, 0),  # Text color
                    2,  # Thickness
                    cv2.LINE_AA,
                )

        # Display the frame with detections
        cv2.imshow("Live Emotion Recognition", frame)

        # Break loop on 'q' key press
        if cv2.waitKey(1) & 0xFF == ord('q'):
            print("Exiting...")
            break

# Release the video capture and close all OpenCV windows
video_capture.release()
cv2.destroyAllWindows()
