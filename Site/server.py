from flask import Flask, render_template, request, session, redirect;
from os.path import join, dirname, realpath
from json import dump, load
from time import asctime

app = Flask(__name__)
app.config['DATA_DIR'] = join(dirname(realpath(__file__)),'static')
app.secret_key = b'99b45274a4b2da7440ab249f17e718688b53b646f3dd57f23a9b29839161749f'    


@app.route("/check_login", methods=['POST'])
def check_login():
    if session.get("connected") :
        return redirect("admin")
    elif request.form["password"] == "ABC": # nope, c'est pas ca le mot de passe :D
        session["connected"] = True
        return redirect("admin")
    else:
        return redirect("/")

@app.route("/disconnect")
def disconnect():
    session["connected"] = False 
    return redirect("/")

@app.route("/send_announcement", methods=['POST'])
def send_announcement():
    if not session.get("connected"):
        return redirect("/")
    with open(join(app.config['DATA_DIR'],"annonces.json"), "r") as file:
        data = load(file)
    annonce_text = request.form["texte"]
    annonce_time = asctime()
    data[f"{len(data)}"] = {"time":annonce_time, "text": annonce_text}
    with open(join(app.config['DATA_DIR'],"annonces.json"), "w") as file:
        dump(data, file)
    return redirect("/")

    

@app.route("/admin_login")
def admin_login():
    if session.get("connected"):
        return redirect("/admin")
    return render_template('admin_login.html')

@app.route("/admin")
def admin():
    if session.get("connected") :
        return render_template('admin.html')
    else:
        return redirect("/")
    
@app.route("/")
def index():
    with open(join(app.config['DATA_DIR'],"annonces.json"), "r") as file:
        data = load(file)
    return render_template('index.html', data=data, lenght=len(data))

app.run(host="127.0.0.1", port=5000, debug=True)