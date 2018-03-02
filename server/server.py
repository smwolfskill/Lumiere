#! /usr/bin/env python2

import os
import os.path

from flask import *
from gevent.wsgi import WSGIServer

log_path = "/home/gitlab-runner/build_logs/"

app = Flask(__name__)

@app.route('/')
def index():
    path = log_path
    files = sorted([f[:-4] for f in os.listdir(path) if os.path.isfile(path + f)])
    return render_template("index.html", files=files)

@app.route('/<log>')
def find_log(log):
    k = log.rfind('/')
    log = log[k + 1:]
    try:
        with open(log_path + log + ".xml") as ff:
            data = ff.read()
    except:
        return abort(404)
    resp = Response(data)
    resp.headers['Content-Type'] = "text/plain"
    return resp

@app.errorhandler(404)
def page_not_found(e):
    return "you failed", 404

if __name__ == '__main__':
    WSGIServer(('', 80), app).serve_forever()

# vim: ts=4:sts=4:sw=4:et
