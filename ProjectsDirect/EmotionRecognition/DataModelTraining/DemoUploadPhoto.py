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
    
    return emotion, valence, arousal

# Resize image while maintaining aspect ratio
def resize_image(image, max_width=800, max_height=800):
    height, width = image.shape[:2]
    if width > max_width or height > max_height:
        scaling_factor = min(max_width / width, max_height / height)
        new_width = int(width * scaling_factor)
        new_height = int(height * scaling_factor)
        resized_image = cv2.resize(image, (new_width, new_height), interpolation=cv2.INTER_AREA)
        return resized_image
    return image

# Initialize Mediapipe Face Detection
mp_face_detection = mp.solutions.face_detection

# Load an image
image_path = '/Users/Owner/OneDrive/Desktop/iteratethrough/Michael_04.jpg'  # Replace with your image path
image = cv2.imread(image_path)

if image is None:
    print(f"Error: Could not load image at {image_path}")
else:
    print(f"Loaded image from {image_path}")

# Resize the image for processing
image = resize_image(image)

# Process the image
with mp_face_detection.FaceDetection(min_detection_confidence=0.5) as face_detection:
    rgb_image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
    results = face_detection.process(rgb_image)

    if results.detections:
        for detection in results.detections:
            bboxC = detection.location_data.relative_bounding_box
            ih, iw, _ = image.shape
            x, y, w, h = (
                int(bboxC.xmin * iw),
                int(bboxC.ymin * ih),
                int(bboxC.width * iw),
                int(bboxC.height * ih),
            )
            x, y, w, h = max(0, x), max(0, y), min(iw - x, w), min(ih - y, h)
            cropped_face = image[y : y + h, x : x + w]

            try:
                emotion, valence, arousal = predict_emotions_with_custom_model(cropped_face)
                print(f"Detected Emotion: {emotion}")
                print(f"Valence: {valence:.2f}, Arousal: {arousal:.2f}")
            except Exception as e:
                print(f"Error during emotion prediction: {e}")
                continue

            # Draw bounding box and label on the image
            # Draw bounding box and annotations on the image
            cv2.rectangle(image, (x, y), (x + w, y + h), (255, 0, 0), 2)
            cv2.putText(image, f"Emotion: {emotion}", (x, y - 20), cv2.FONT_HERSHEY_SIMPLEX, 0.6, (255, 0, 0), 2)
            cv2.putText(image, f"Val: {valence:.2f}", (x, y + 20), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (255, 255, 0), 1)
            cv2.putText(image, f"Aro: {arousal:.2f}", (x, y + 30), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (255, 255, 0), 1)


        # Show the result
        cv2.imshow("Emotion Recognition", image)
        cv2.waitKey(0)  # Wait indefinitely until a key is pressed
    else:
        print("No face detected in the image.")

# Release resources
cv2.destroyAllWindows()