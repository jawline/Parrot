!=!=! Title: Detecting Humanity - Brave Software
!=!=! Created: 1533833028.846177
!=!=! Tags: Work, Research

!=!=! Intro: Start
During a research internship at Brave Software in 2018 I worked on verifiable client side humanity detection tools. This work is a deployment of anomaly detection to identify client humanity through runtime behaviour. Humanity detection is an issue in software development, where automation tools have forced deployment of obtrusive countermeasures (e.g. CAPTCHA). The biggest concerns raised by this approach are privacy and verifiability, as the proof of humanity is on client side but must be verifiable by a third party (Brave) without exposing private user data.
!=!=! Intro: End

As part of the work I developed a proxy capable of instrumenting webpages to provide streamed runtime user behaviour. A sample dataset of real human behaviour was then developed in-house. Several well known automation tools, such as ZennoPoster and uBot, were used to generate negative data for an evaluation of the approach. The approach was evaluated through a pre-existing 1-class anomaly detection framework.

Anomaly detection is a heuristic approach to outlier detection. An advantage of many anomaly detection schemes a requirement for only positive training data. When developing a humanity detection tool this flavor of anomaly detection provides a key advantage. The effectiveness of existing automation tool detection is limited by the behaviour of known malware samples - as a result an attacker need only sufficiently differentiate its behaviour from the training dataset in order to circumvent the detection system. As this approach only requires positive behaviour the attacker now has to make their behaviour appear human, rather than just differentiate its behaviour from other automation tools.

An evaluation of my work showed that this approach can identify all evaluated automation tools distinctly from humans with a reasonable false positive rate. As runtime behaviour is streamed any false positives can be mitigated through continued re-classification.
