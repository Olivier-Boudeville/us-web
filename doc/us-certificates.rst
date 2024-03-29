
--------------------------------
Managing Public-Key Certificates
--------------------------------

The goal here is to benefit from suitable certificates, notably (but not only) for https (typically running on TCP port 443, multiplexed thanks to SNI, i.e. `Server Name Indication <Server Name Indication>`_ [#]_), by automatically (and freely) generating, using and renewing them appropriately, for each of the virtual hosts managed by the US-Web server.


.. [#] As a consequence, the specific visited virtual hostname (e.g. ``baz``, in  ``baz.foobar.org``) is *not* encrypted, and thus might be known of an eavesdropper.



X.509 Certificates
==================

The certificates managed here are `X.509 <https://en.wikipedia.org/wiki/X.509>`_ TLS certificates, which can be seen as standard containers of a public key together with an identity and a hierarchical, potentially trusted *Certificate Authority* (CA) that signed them [#]_.

.. [#] The X.509 standard also includes certificate revocation lists and the algorithm to sign recursively certificates from a trust anchor.

Such certificates can be used for any protocol or standard, and `many <https://en.wikipedia.org/wiki/X.509#Major_protocols_and_standards_using_X.509_certificates>`_ do so - including of course TLS and, to some extent, SSH. Being necessary to the https scheme, they are used here.



Let's Encrypt
=============

US-Web relies on `Let's Encrypt <https://letsencrypt.org>`_, a non-profit certificate authority from which one can obtain mono-domain (possibly with SANs - *Subject Alternative Names*; and now wildcard domains) X.509 certificates [#]_ for free, valid for 90 days and that can be renewed as long as needed.

.. [#] ``Let's Encrypt`` provides *Domain Validation* (DV) certificates, but neither more general *Organization Validation* (OV) nor *Extended Validation* (EV).

Let's Encrypt follows the ACME (*Automatic Certificate Management Environment*) protocol. For mono-domain validation, it relies on an ``http-01`` challenge, with an agent running on the server bound to the domain for which a certificate is requested.

This agent generates first a RSA key pair in order to interact with the Let’s Encrypt certificate authority, so that it can prove through received challenge(s) that it is bound to the claimed domain / virtual host (e.g. ``baz.foobar.org``) and has the control to the private key corresponding to the public one that it transmitted to the CA.

Generally this involves for that agent to receive a "random" piece of data from the CA (the nonce), to sign it with said private key, and to make the resulting answer to the challenges available (as tokens) through the webserver at a relevant URL that corresponds to the target virtual host and to a well-known path (e.g. ``http://baz.foobar.org/.well-known/acme-challenge/xxx``).

Refer to `this page <https://letsencrypt.org/how-it-works/>`_ for more information; the overall process is `explained here <https://ietf-wg-acme.github.io/acme/draft-ietf-acme-acme.html#rfc.section.4>`_ and in `this RFC <https://www.rfc-editor.org/rfc/rfc8555.html>`_.

As for wildcard certificates, the ACME protocol relies on ``dns-01`` challenges, which require the certificate requester to prove that it controls the DNS of the target domain, by updating its zone entries according to information chosen by the ACME server of the certificate issuer.



US-Web Mode of Operation
========================


For mono-domain certificates
----------------------------

Rather than using a standalone ACME agent such as the standard one, ``certbot``, we prefer driving everything from Erlang, for a better control and periodical renewal (see the scheduler provided by `US-Common <https://github.com/Olivier-Boudeville/us-common/blob/master/src/class_USScheduler.erl>`_).

Various libraries exist for that in Erlang, the most popular one being probably `letsencrypt-erlang <https://github.com/gbour/letsencrypt-erlang>`_; we `forked <https://github.com/Olivier-Boudeville/letsencrypt-erlang>`_ it (and named it LEEC, for *Let's Encrypt Erlang with Ceylan*, to tell them apart), in order notably to support newer Erlang versions and to allow for *concurrent* certificate renewals (knowing that one certificate per virtual host will be needed).

Three `action modes <https://github.com/Olivier-Boudeville/letsencrypt-erlang#action-modes>`_ can be considered to interact with the Let's Encrypt infrastructure and to solve its challenges. As the US-Web server is itself a webserver, the ``slave`` mode is the most relevant here.

For that, the `us_web_letsencrypt_handler <https://github.com/Olivier-Boudeville/us-web/blob/master/src/us_web_letsencrypt_handler.erl>`_ has been introduced by US-Web.

By default, thanks to the US-Web scheduler, certificates (which last for up to 90 days and cannot be renewed before 60 days are elapsed) will be renewed every 75 days, with some random jitter added to avoid synchronising too many certificate requests when having multiple virtual hosts - as they are done concurrently, if not SANs are used.


For wildcard certificates
-------------------------

US-Web relies here also as much as possible on LEEC (see `this section <https://leec.esperide.org/#wildcard-domain-certificates-with-the-dns-01-challenge>`_), even if, at least currently, the standard ``certbot`` is used internally; here US-Web, thanks to its US-Common Scheduler, acts mostly like an integrated crontab on steroids.



Settings
========

Various `types of files <https://crypto.stackexchange.com/questions/43697/what-is-the-difference-between-pem-csr-key-and-crt-and-other-such-file-ext>`_ are involved in the process:

- a ``.key`` file contains any type of key, here this is a RSA private key; typically ``letsencrypt-agent.key-I``, where ``I`` is an increasing integer, will contain the PEM RSA private key generated by the certificate agent ``I`` on behalf of the US-Webserver (so that it can sign the nonces provided by Let's Encrypt, and thus prove that it controls the corresponding key pair); for a ``baz.foobar.org`` virtual host, the ``baz.foobar.org.key`` file will be generated and used (another PEM RSA private key)
- a ``.pem`` (*Privacy-enhanced Electronic Mail*) file just designates a Base64-encoded content with header and footer lines; here it stores an ASN.1 (precisely a Base64-encoded DER) certificate
- ``.csr`` corresponds to a PKCS#10 *Certificate Signing Request*; it contains information (encoded as PEM or DER) such as the public key and common name required by a Certificate Authority to create and sign a certificate for the requester (e.g. ``baz.foobar.org.csr`` will be a PEM certificate request)
- ``.crt`` is the actual certificate (encoded as PEM or DER as well), usually a X509v3 one (e.g. ``baz.foobar.org.crt``); it contains the public key and also much more information (most importantly the signature by the Certificate Authority over the data and public key, of course)


We must determine:

- the size of the RSA key of the agent; the higher the better, hence: 4096
- where the certificate-related files will be stored: in the ``certificates`` subdirectory of the US-Web data directory, i.e. the one designated by the ``us_web_data_dir`` key in US-Web's configuration file (hence it is generally the ``/var/local/us-web/us-web-data`` or ``/opt/universal-server/us_web-x.y.z/us-web-data`` directory)


The precise support for X.509 certificates (use, and possibly generation and renewal) is determined by the ``certificate_support`` key of the US-Web configuration file:

- if not specified, or set to ``no_certificates``, then no certificate will be used, and thus no https support will be available
- if set to ``use_existing_certificates``, then relevant certificates are supposed to pre-exist, and will be used as are (with no automatic renewal thereof done by US-Web)
- if set to ``renew_certificates``, then relevant certificates will be generated at start-up (none re-used), used since then, and will be automatically renewed whenever appropriate


Another setting applies, determined this time by the ``certificate_mode`` key, whose associated value (which matters iff ``certificate_support`` has been set to ``renew_certificates``) is either ``development`` or ``production`` (the default). In the former case, the `staging ACME parameters <https://letsencrypt.org/docs/staging-environment/>`_ will apply (implying relaxed limits, yet resulting in the obtained certificates to be issued by a fake, test CA), whereas the latter case will imply the use of the `production <https://letsencrypt.org/docs/rate-limits/>`_ ones.

.. comment letsencrypt-erlang must be able to write on the webserver, at the root of the website.


When a proper certificate is available and enabled, the US webserver promotes automatically any HTTP request into a HTTPS one, see the `us_web_port_forwarder <https://github.com/Olivier-Boudeville/us-web/blob/master/src/us_web_port_forwarder.erl>`_ module for that (based on relevant routing rules).


Standard, basic firewall settings are sufficient to enable interactions of US-Web (through LEEC) with Let's Encrypt, as it is the US-Web agent that initiates the TCP connection to Let's Encrypt, which is to check the challenge(s) through regular http accesses done to the webserver expected to be available at the domain of interest.

The US-Web server must be able to write in the web content tree, precisely to write files in the ``well-known/acme-challenge/`` subdirectory of the web root.


HTTPS Troubleshooting
=====================

If trying to connect with the https scheme whereas it has not been enabled, ``wget https://baz.foobar.org/ -O -`` is to report ``Connection refused``.
