<div align="center">
  <a href="https://cs
<div align="center">
  <a href="https://stabl.rocks/ShowModulePublish?modulePublishId=0a6330dc-6e05-447c-820f-293aca08929a&fullscreen=true">
    <img src="https://github.com/user-attachments/assets/3c9585d6-5fae-4c68-bd85-05d2bfdd3e3c" alt="Logo" width="80" height="80">
  </a>

<h3 align="center">Binary 0101</h3>

  <p align="center">
    An interactive educational module designed to teach the fundamentals of binary numbers and their applications.
    <br />
     <a href="https://stabl.rocks/ShowModulePublish?modulePublishId=0a6330dc-6e05-447c-820f-293aca08929a&fullscreen=true">cs1xd3.online</a>
  </p>
</div>

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <!--<li><a href="#demo">Demo</a></li> -->
        <li><a href="#key-features">Key Features</a></li>
      </ul>
    </li>
    <li><a href="#architecture">Architecture</a></li>
    <li><a href="#getting-started">Getting Started</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>

## About The Project

Binary 0101 is an interactive learning module created for CS1XD3, designed to introduce users to the binary number system and its applications in computer science. The module covers topics such as binary-decimal conversion, binary arithmetic, and the use of binary in representing text (ASCII). It features a menu-driven interface with various interactive visuals and practice exercises.

<!--### Demo

<div align="center">
  <a href="https://cs1xd3.online/ShowModulePublish?modulePublishId=0a6330dc-6e05-447c-820f-293aca08929a&fullscreen=true">
    <img src="https://github.com/user-attachments/assets/3b6baea2-cb25-4670-86b8-094d69d2bf83" alt="Project Demo">
  </a>
</div> -->

### Key Features

- **Interactive Lessons:** Step-by-step lessons covering the basics of binary numbers, conversion methods, and applications.
- **Visual Aids:** Includes interactive animations and visualizations to help users understand binary concepts.
- **Practice Exercises:** Offers practice questions for binary-to-decimal and decimal-to-binary conversion.
- **ASCII Converter:** Allows users to convert text to decimal and binary ASCII representations.
- **Menu-Driven Interface:** Easy navigation through the different lessons and modules.

## Architecture

<!-- ![Architecture Diagram](https://github.com/user-attachments/assets/79d3c0f6-21b6-413b-9f30-5117c6b60e7d) -->

- **Frontend:**
  - Elm: The entire project is built using the Elm programming language, a functional language known for its reliability and maintainability.
  - Collage: Elm's collage library is used for creating the visual elements and animations.
  - Interactive Components: Custom-built interactive components for lessons and practice exercises.
- **Modules:**
  - `BinaryApp.elm`: Main application module that orchestrates the different lessons and modules.
  - `BinaryMenu.elm`: Module for the main menu, providing navigation to different lessons.
  - `RandBinDec.elm`: Generates random binary and decimal numbers for demonstration.
  - `LightVisualization.elm`: Interactive visualizer with 8 lights representing an 8-bit binary number.
  - `NewVisualizerColumnAnimation.elm`: Animates the column breakdown of decimal and binary numbers.
  - `NewBin2DecVisual.elm`: Demonstrates the conversion of binary to decimal.
  - `NewDecimalToBinaryAnimation.elm`: Demonstrates the conversion of decimal to binary.
  - `Bin2DecPractice.elm`: Practice questions for binary to decimal conversion.
  - `Decimal2BinaryPractice.elm`: Practice questions for decimal to binary conversion.
  - `Switch.elm`: Visual representation of a computer switch to illustrate binary.
  - `SingleLight.elm`: Single light bulb visual to represent a binary bit.
  - `HexCol.elm`: Generates random colors and displays their hexadecimal and RGB values.
  - `ASCII.elm`: Converts text to decimal and binary ASCII values.
  - `ExtraApps.elm`: Displays binary addition, multiplication, and conversion to octal.
  - `Credits.elm`: End visual with a congratulations message and certificate animation.
  - `GetArrowKeys.elm`: Captures arrow key inputs for lesson navigation.
  - `RandBinNum.elm`: Generates random numbers for practice modules.

## Getting Started

Unfortunately, since this was a school project, it had to be created using a WebIDE which hid certain code in some files. Therefore, it would be impossible to download the files and run the project locally.

## Acknowledgments

- This README was generated with [README Generator](https://github.com/owengretzinger/readme-generator) — an AI tool that understands your entire codebase.
- This project was created by Owen Gretzinger, Hilary He, Vicky Robinson, Louis Doan, and Victor Moucattash for CS1XD3.
