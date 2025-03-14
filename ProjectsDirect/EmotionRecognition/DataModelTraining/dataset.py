from PIL import Image
from torch.utils.data import Dataset
import numpy as np
import torch

import os
import torch
import pandas as pd
import numpy as np
from PIL import Image
from torch.utils.data import DataLoader
from torchvision import transforms
import torch.nn as nn


class CustomEmotionDataset(torch.utils.data.Dataset):
    def __init__(self, csv_file, root, transform, split=None):
        """
        Args:
            csv_file (str): Path to the metadata CSV file.
            root (str): Root directory for images.
            transform (callable): Transformations to apply to the images.
            split (str, optional): Dataset split to filter (e.g., "Train", "Validation", "Test").
        """
        df = pd.read_csv(csv_file)

        # Filter by split if provided
        if split is not None:
            df = df[df['split'] == split]

        # Ensure we only keep valid emotion classes
        df = df[df['emotion'].notna()]  # Filter out rows with NaN emotion
        self.paths = df['image_path'].tolist()  # List of image paths
        self.targets = df['emotion'].to_numpy(dtype=np.int64)  # Emotion labels
        # self.valence_arousal = df[['valence', 'arousal']].to_numpy(dtype=np.float32)  # Valence and arousal
        self.transform = transform
        self.root = root

    def __len__(self):
        return len(self.paths)

    def __getitem__(self, idx):
    # Normalize and use image path directly without appending root
        img_path = os.path.normpath(self.paths[idx])  # Normalize path to avoid redundant slashes
        if img_path.startswith('./') or img_path.startswith('../'):  # If relative, append root
            img_path = os.path.join(self.root, img_path.lstrip('./'))

        img = Image.open(img_path).convert('RGB')
        img = self.transform(img)

        # Get labels
        emotion_label = self.targets[idx]
        # valence = torch.tensor(self.valence_arousal[idx, 0], dtype=torch.float32)
        # arousal = torch.tensor(self.valence_arousal[idx, 1], dtype=torch.float32)

        return img, (emotion_label)



