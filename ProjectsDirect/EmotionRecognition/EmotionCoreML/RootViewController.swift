//
//  RootViewController.swift
//  Project Emotion Recognition
//
//  Created by Michael Guel on 11/16/24.
//

import UIKit

class RootViewController: UIViewController {

    // MARK: - IBOutlets
    @IBOutlet weak var openCameraButton: UIButton!

    override func viewDidLoad() {
        super.viewDidLoad()
        print("RootViewController loaded.")
        // Additional setup if needed
    }

    // MARK: - IBActions
    @IBAction func openCameraTapped(_ sender: UIButton) {
        print("Open Camera button tapped!")
        performSegue(withIdentifier: "openCameraSegue", sender: self)
    }
}
