!=!=! Title: A Solution To Compression Oracles on the Web - Cloudflare
!=!=! Created: 1533833022.682019
!=!=! Tags: Work, Research

!=!=!
Compression is often considered an essential tool when reducing the bandwidth usage of internet services. The impact that the use of such compression schemes can have on security, however, has often been overlooked. The recently detailed CRIME, BREACH, TIME and HEIST attacks on TLS have shown that if an attacker can make requests on behalf of a user then secret information can be extracted from encrypted messages using only the length of the response. Deciding whether an element of a web-page should be secret often depends on the content of the page, however there are some common elements of web-pages which should always remain secret such as Cross-Site Request Forgery (CSRF) tokens. Such tokens are used to ensure that malicious webpages cannot forge requests from a user by enforcing that any request must contain a secret token included in a previous response.
!=!=!

I worked at Cloudflare last summer to investigate possible solutions to this problem. The result is a project called cf-nocompress. The aim of this project was to develop a tool which automatically mitigates instances of the attack, in particular CSRF extraction, on Cloudflare hosted services transparently without significantly impacting the effectiveness of compression. We have published a proof-of-concept implementation on GitHub, and provide a challenge site and tool which demonstrates the attack in action).

### Resources

[A full account of my work can be read on the Cloudflare blog](https://blog.cloudflare.com/a-solution-to-compression-oracles-on-the-web/)
