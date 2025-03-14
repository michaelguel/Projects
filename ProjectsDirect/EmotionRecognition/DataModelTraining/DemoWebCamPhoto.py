import torch
import cv2
import mediapipe as mp
import numpy as np

# Load your custom-trained model
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
model = torch.load('/Users/Owner/OneDrive/Desktop/Mine_enet_b0_7_va_mtl.pt', map_location=device)
model = model.to(device)
model.eval()  # Set model to evaluation mode

# Define a function to preprocess the face image and predict emotions
def predict_emotions_with_custom_model(cropped_face):
    resized_face = cv2.resize(cropped_face, (260, 260))  # Assuming 224x224 input size
    face_tensor = torch.tensor(resized_face, dtype=torch.float32).permute(2, 0, 1)  # HWC to CHW
    face_tensor = face_tensor.unsqueeze(0).to(device) / 255.0  # Normalize to [0, 1]
    
    with torch.no_grad():
        scores = model(face_tensor)  # Get all scores
        scores = scores[0].data.cpu().numpy()  # Convert to NumPy array
    
    # Emotion prediction
    emotion_labels = ['Neutral', 'Happiness', 'Sadness', 'Surprise', 'Fear', 'Disgust', 'Anger']
    emotion_scores = scores[:-2]  # First 7 elements
    emotion_index = np.argmax(emotion_scores)
    emotion = emotion_labels[emotion_index]
    
    # Valence and arousal
    valence, arousal = scores[-2:]  # Last two elements
    
    return emotion, emotion_scores, valence, arousal

# Initialize Mediapipe Face Detection
mp_face_detection = mp.solutions.face_detection

# Open webcam video stream
video_capture = cv2.VideoCapture(0)  # Default webcam

# Check if the webcam is opened successfully
if not video_capture.isOpened():
    print("Error: Could not access the webcam.")
else:
    print("Webcam accessed successfully. Press 'c' to capture a photo or 'q' to quit.")

# Process single photo
with mp_face_detection.FaceDetection(min_detection_confidence=0.5) as face_detection:
    while True:
        ret, frame = video_capture.read()
        if not ret:
            print("Failed to grab frame. Exiting...")
            break
        
        cv2.imshow("Press 'c' to capture a photo", frame)

        key = cv2.waitKey(1) & 0xFF
        if key == ord('c'):  # Capture photo
            rgb_frame = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
            results = face_detection.process(rgb_frame)

            if results.detections:
                for detection in results.detections:
                    bboxC = detection.location_data.relative_bounding_box
                    ih, iw, _ = frame.shape
                    x, y, w, h = (
                        int(bboxC.xmin * iw),
                        int(bboxC.ymin * ih),
                        int(bboxC.width * iw),
                        int(bboxC.height * ih),
                    )
                    x, y, w, h = max(0, x), max(0, y), min(iw - x, w), min(ih - y, h)
                    cropped_face = frame[y : y + h, x : x + w]

                    try:
                        emotion, emotion_scores, valence, arousal = predict_emotions_with_custom_model(cropped_face)
                        print(f"Emotion: {emotion}, Valence: {valence:.2f}, Arousal: {arousal:.2f}")
                    except Exception as e:
                        print(f"Error during emotion prediction: {e}")
                        continue

                    # Draw bounding box and label on the captured frame
                    cv2.rectangle(frame, (x, y), (x + w, y + h), (255, 0, 0), 2)
                    cv2.putText(frame, f"{emotion}", (x, y - 30), cv2.FONT_HERSHEY_SIMPLEX, 0.8, (255, 0, 0), 2)
                    cv2.putText(frame, f"Val: {valence:.2f}", (x, y - 10), cv2.FONT_HERSHEY_SIMPLEX, 0.6, (255, 0, 0), 2)
                    cv2.putText(frame, f"Aro: {arousal:.2f}", (x, y + 10), cv2.FONT_HERSHEY_SIMPLEX, 0.6, (255, 0, 0), 2)

                    cv2.imshow("Captured Photo", frame)
                    cv2.waitKey(0)  # Wait indefinitely until a key is pressed
            else:
                print("No face detected in the captured photo.")

        elif key == ord('q'):  # Quit
            print("Exiting...")
            break

# Release resources
video_capture.release()
cv2.destroyAllWindows()
