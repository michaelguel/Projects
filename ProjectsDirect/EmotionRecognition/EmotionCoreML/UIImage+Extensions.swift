//
//  UIImage+Extensions.swift
//  EmotionCoreML
//
//  Created by Michael Guel on 12/10/24.
//

import UIKit


extension UIImage {
    /// Normalizes the image orientation to `.up`.
    func normalizedImage() -> UIImage {
        if imageOrientation == .up {
            return self
        }
        UIGraphicsBeginImageContextWithOptions(size, false, scale)
        draw(in: CGRect(origin: .zero, size: size))
        guard let normalizedImage = UIGraphicsGetImageFromCurrentImageContext() else {
            UIGraphicsEndImageContext()
            return self
        }
        UIGraphicsEndImageContext()
        return normalizedImage
    }

    /// Converts `UIImageOrientation` to `CGImagePropertyOrientation`.
    var cgImagePropertyOrientation: CGImagePropertyOrientation {
        switch imageOrientation {
        case .up: return .up
        case .down: return .down
        case .left: return .left
        case .right: return .right
        case .upMirrored: return .upMirrored
        case .downMirrored: return .downMirrored
        case .leftMirrored: return .leftMirrored
        case .rightMirrored: return .rightMirrored
        @unknown default:
            return .up
        }
    }
}
