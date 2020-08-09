
--------------------------------
Managing Public-Key Certificates
--------------------------------

The goal here is to benefit from suitable certificates, notably for https (typically running on TCP port 443), by automatically (and freely) generating, using and renewing them appropriately, for each of the virtual hosts managed by the US-Web server.


X.509 Certificates
==================

These are `X.509 <https://en.wikipedia.org/wiki/X.509>`_ TLS certificates, which can be seen as standard containers of a public key together with an identity and a hierarchical, potentially trusted Certificate Authority (CA) that signed them [#]_.

.. [#] The X.509 standard also includes certificate revocation lists and the algorithm to sign recursively certificates from a trust anchor.

Such certificates can be used for any protocol or standard, and `many <https://en.wikipedia.org/wiki/X.509#Major_protocols_and_standards_using_X.509_certificates>`_ do so - including of course TLS and, to some extent, SSH.


Let's Encrypt
=============

For that US-Web relies on `Let's Encrypt <https://letsencrypt.org>`_, from which one can obtain mono-domain certificates [#]_ for free, valid for 90 days and that can be renewed as often as needed.

.. [#] ``Let's Encrypt`` provides *Domain Validation* (DV) certificates, but not more general *Organization Validation* (OV) or *Extended Validation* (EV).

Let's Encrypt follows the ACME (*Automatic Certificate Management Environment*) protocol. It relies on an agent running on the server bound to the domain for which a certificate is requested. This agent owns a RSA key pair in order to interact with the Let’s Encrypt CA (*Certificate Authority*) so that it can prove that it is bound to the claimed domain and has the control to the private key.

Generally it involves for that agent to receive a "random" piece of data from the CA, to encrypt it with said private key, and to make the resulting file available through the webserver at specified domain (see `this <https://letsencrypt.org/how-it-works/>`_ for more information) that can then be checked with the RSA public key associated to this domain.

Rather than using a standalone ACME agent such as the standard one, ``certbot``, we prefer driving everything from Erlang, for a better control and scheduling (see the scheduler provided by `US-Common <https://github.com/Olivier-Boudeville/us-common/blob/master/src/class_USScheduler.erl>`_).

Various libraries exist for that in Erlang, the most popular one being probably `letsencrypt-erlang <https://github.com/gbour/letsencrypt-erlang>`_ (we also `forked <https://github.com/Olivier-Boudeville/letsencrypt-erlang>`_ it).


Mode of Operation
=================

Three `action modes <https://github.com/gbour/letsencrypt-erlang#action-modes>`_ can be considered to interact with the Let's Encrypt infrastructure. As the US-Web server is itself a webserver, the ``slave`` mode is the most relevant here.

For that, the `us_web_letsencrypt_handler <https://github.com/Olivier-Boudeville/us-web/blob/master/src/us_web_letsencrypt_handler.erl>`_ has been introduced.

By default, thanks to the US-Web scheduler, certificates (which last for up to 90 days and cannot be renewed before 60 days are elapsed) will be renewed every 75 days (with some random jitter added to avoid synchronising too many certificate requests when having multiple virtual hosts).



Settings
========

We must determine:

- the size of the RSA key of the agent; the higher the better, hence: 4096
- where the RSA key pair will be stored; it is to be placed in the ``certificates`` subdirectory of the US-Web data directory, i.e. the one designated by the ``us_web_data_dir`` key in US-Web's configuration file (hence it is generally the default ``/var/local/us-web/data`` directory)


A ``.pem`` (*Privacy-enhanced Electronic Mail*) file stores an ASN.1 (precisely a Base64-encoded DER) certificate.

The support for X.509 certificates (use, and possibly generation and renewal) is determined by the ``certificate_support`` key of the US-Web configuration file:

- if not specified or set to ``no_certificates``, then no certificate will be used, and thus no https will be available
- if set to ``use_existing_certificates``, then relevant certificates are supposed to pre-exist, and will be used as are (with no automatic renewal thereof done by US-Web)
- if set to ``renew_certificates``, then relevant certificates will be generated at start-up, used since then, and will be automatically renewed whenever appropriate

.. comment letsencrypt-erlang must be able to write on the webserver, at the root of the website.

When a proper certificate is available, the US webserver shall automatically promote any HTTP request into a HTTPS one, see the `us_web_port_forwarder <https://github.com/Olivier-Boudeville/us-web/blob/master/src/ us_web_port_forwarder.erl`_ module for that (based on relevant routing rules).


.. Possible causes of errors: - firewall filtering
