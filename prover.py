import sys
import json
import datetime
import traceback
import subprocess
import base64
from flask import Flask
from flask import request

HOST = 'localhost'
PORT = 5007
app = Flask(__name__)


@app.route('/prove', methods=['POST'])
def prove():
    ret_value = {"result": {"errors": None, "output": None, "return_string": None}}
    data = request.get_json()

    if len(data) == 0 or not data["prove"]:
        print("No input text found")
        ret_value["result"]["errors"] = "No input text found"
        sys.stdout.flush()
        return json.dumps(ret_value)

    print("Text to process: {0}".format(data))
    try:
        deontic = data["deontic"]
        prove = data["prove"]
        output_format = data["format"]
        proc = f"bash exec.sh '{prove}' '' '{deontic}' '' '' '' '' '' 'standardoperators' 'modern' '{output_format}'"
        print(proc)
        outp = subprocess.check_output(proc, shell=True, text=True, timeout=60)

        outp = outp.split(";")
        return_string = ""
        if outp[0] == "0":
            return_string = "These seems to have been a syntax problem - Please check your input!"
        elif outp[0] == "1":
            return_string = "The derivation is too big to show :("
        elif outp[0] == "3":
            return_string = "Not derivable"
        else:
            outp_pdf = outp[1].strip("\n")

            if output_format == "derivation":
                with open(outp_pdf, "rb") as pdf_file:
                    encoded_string = base64.b64encode(pdf_file.read())
                ret_value["result"]["output"] = encoded_string.decode("utf-8")
            else:
                with open(outp_pdf, "r") as pdf_file:
                    encoded_string = pdf_file.read()
                ret_value["result"]["output"] = encoded_string

            #return_string = f'<center><object data=\"" . $output_arr[1] . "\" type=\"application/pdf\" width=\"100%\" height=\"100%\"> Please download the PDF  <a href=\"" . {outp[1]} . "\">here</a>.</object></center> '

        ret_value["result"]["result_string"] = return_string

    except Exception as e:
        traceback.print_exc()
        ret_value["result"]["errors"] = str(e)

    #print("Returning: {0}".format(ret_value))
    sys.stdout.flush()
    return json.dumps(ret_value)


if __name__ == '__main__':
    app.run(debug=True, host=HOST, port=PORT)