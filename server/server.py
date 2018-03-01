#! /usr/bin/env python2

import os
import os.path

from flask import *

app = Flask(__name__)

@app.route('/')
def index():
    path = "/root/build_logs/"
    files = sorted([f[:-4] for f in os.listdir(path) if os.path.isfile(path + f)])
    return render_template("index.html", files=files)

@app.route('/<log>')
def find_log(log):
    k = log.rfind('/')
    log = log[k + 1:]
    with open("/root/build_logs/" + log + ".xml") as ff:
        data = ff.read()
    resp = Response(data)
    resp.headers['Content-Type'] = "text/plain"
    return resp

@app.errorhandler(404)
def page_not_found(e):
    return "you failed", 404

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=80)

# vim: ts=4:sts=4:sw=4:et
