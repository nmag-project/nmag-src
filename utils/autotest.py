
#!/usr/bin/env python

nmag_bigbar_resultsdir='.'
#cvs_repository = 'alpha.sesnet.soton.ac.uk:/var/local/cvs/tf'
svn_repository = 'svn+ssh://alpha.kk.soton.ac.uk/var/local/svn/nsim/trunk'
deviationlogfilenmag20070328 = 'deviationnmag3_20070328.txt'
deviationlogfileoommf = 'deviationoommf.txt'

fromAddress = 'nmagtest@eta.kk.soton.ac.uk'
emailSMTP = 'smtp.soton.ac.uk'


import logging,os,sys,time,optparse
log = logging

logging.getLogger('').setLevel(logging.DEBUG)

from os.path import join as join


timings = {}

def attempt(command,ignore_error = False):
    log.debug("About to execute: '%s'" % command)
    exitcode = os.system(command)
    
    if exitcode != 0:
        if not ignore_error:
            sys.stderr.write("An error occured, leaving now\n(command was %s)\n" % command)
            print "Timings so far:",str(timings)
            sys.exit(1)
    return exitcode


if not os.path.exists(nmag_bigbar_resultsdir):
    attempt('mkdir -p '+nmag_bigbar_resultsdir)


#make sure there is no old ocaml
if os.path.exists('./nsim'):
    log.debug('removing old "nsim"')
    attempt('rm -rf nsim')


def checkout(date=None,headbigbar=False):
    """date format is (year,month,day,hour,minute))"""

    if date:
        (year,month,day,hour,minute) = date
        
        dirname = '%04d-%02d-%02d-%02d%02d' % date
        #cvsname = '%04d-%02d-%02d %02d:%02d' % date
        svnname = '%04d%02d%02dT%02d%02d' % date

        #cvsdatecommand = '-D "'+cvsname+'"'
        svndatecommand = '-r {'+svnname+'}'

    else: #most current
        dirname = time.strftime('%Y-%m-%d-%H%M')
        #cvsdatecommand = ''
        svndatecommand = ''

    #checkout_command = 'cvs -d '+cvs_repository+' co '+cvsdatecommand+' ocaml'

    checkout_command = 'svn co '+svndatecommand+' '+svn_repository+' nsim'

    attempt(checkout_command)
    attempt('mv nsim '+dirname)

    #Need also to check out nmag3 deviation business
    #checkout_command = 'cvs -d '+cvs_repository+' co ocaml/tests/regression/nmag/bigbar/results/nmag_deviation.py'
    checkout_command = 'svn cat '+svn_repository+'/tests/regression/nmag/bigbar/results/nmag_deviation.py > '+dirname+'/tests/regression/nmag/bigbar/results/nmag_deviation.py'

    #check out the current version
    checkout_command = 'svn co '+svn_repository+' nsimcurrent '
    attempt(checkout_command)

    if headbigbar:

        #sometimes, we'd like to use the most recent bigbar.py
        #checkout_command = 'cvs -d '+cvs_repository+' co ocaml/tests/regression/nmag/bigbar'
        checkout_command = 'svn cat '+svn_repository+'/tests/regression/nmag/bigbar tests/regression/nmag'
        attempt(checkout_command)
    

    return dirname


def compile(dirname):

    """Note: this bizzare sequnce of make commands has been
    chosen to make the code compile while the Thomas' ddiffop_parser
    bug is in the make file. Annoying. (fangohr 26/03/2007)
    """
    command = 'cd '+dirname+'; make config/configuration.inc'
    command = 'cd '+dirname+'; make'
    attempt(command,ignore_error=True)
    command = 'cd '+dirname+'; cd nsim_grammars; make ddiffop_parser.cma'
    attempt(command,ignore_error=True) #older versions don't have nsim_grammars
    command = 'cd '+dirname+'; make'
    attempt(command,ignore_error=True)
    command = 'cd '+dirname+'; make'
    attempt(command)


def runtest_bigbar(dirname,headbigbar=False):

    bigbardir = join(dirname,'tests/regression/nmag/bigbar')
    if headbigbar:
        command = 'cp nsimcurrent/tests/regression/nmag/bigbar/nmag4/bigbar.py '+join(bigbardir,'nmag4')
        attempt(command)
    
    command = 'cd '+bigbardir+'; make check'
    attempt(command)

    command = 'gzip '+join(bigbardir,'results','results.eps')
    attempt(command)
    
    for filename in ['results.eps.gz','results.png','nmag4_M.dat']:
        command = 'cp '+join(bigbardir,join('results',filename)) \
                  +' '+join(nmag_bigbar_resultsdir,dirname+'_'+filename)
        attempt(command)
        

def delete_build_dir(dirname):
    log.debug("About to rm -rf %s" % dirname)
    attempt('rm -rf '+dirname)


def whoami():
    return os.popen('whoami').read()

def hostname():
    return os.popen('hostname').read()

def uname():
    return os.popen('uname -a').read()

def create_info_file(dirname,timings):
    msg =  "SVN from : %s\n" % dirname
    msg += "The test was run:\n"
    msg += "time     : %s\n" % (time.asctime())
    msg += "host     : %s" % (hostname())
    msg += "user     : %s" % (whoami())
    msg += "getcwd   : %s\n" % (os.getcwd())
    msg += "uname -a : %s" % (uname())
    msg += "Id       : $Id$ (the file running the test)\n\n"
    msg += "Timings  :\n"
    mykeys = timings.keys()
    mykeys.sort()
    for key in mykeys:
        msg += "  %15s : %5.5f seconds\n" % (key,timings[key])

    f=open(dirname+'_info.txt','w')
    f.write(msg)
    f.close()

def append_deviation_data(dirname):
    filename=deviationlogfilenmag20070328

    #if not os.path.exists(filename):
    #    attempt('touch '+filename)

    exitcode = attempt('python nsimcurrent/tests/regression/nmag/bigbar/results/nmag_deviation.py'\
                       +' '\
                       +join(dirname,'tests/regression/nmag/bigbar/results/nmag4_M.dat')+' '+dirname+' "(nmag4)" --nmag3_20070328'\
                       +' >> '+filename)
    
    #if exitcode != 0:
    #    #this could be because the nmag3deviation script didn't exist in earliear versions
    #    #in that case, use most recent version
    #
    #    log.debug("nmag3deviation.py failed, probably because the command didn't exist at the time")
    #    log.debug("nmag3deviation.py failed: nasty hack: will try version in cvs/ocaml/...")
    #    
    #    exitcode = attempt('python '\
    #                       +join('~/cvs/ocaml/tests/regression/nmag/bigbar/results/nmag3deviation.py')\
    #                       +' '\
    #                       +join(dirname,'tests/regression/nmag/bigbar/results/nmag3_M')+' '+dirname+' "(nmag)" --nmag3_20070328'\
    #                       +' >> '+filename)

    #append empty line to make file easier to read
    attempt('''python -c "print '#'" >>  '''+filename)



    filename=deviationlogfileoommf
            
    exitcode = attempt('python '\
                       +'nsimcurrent/tests/regression/nmag/bigbar/results/nmag_deviation.py'\
                       +' '\
                       +join(dirname,'tests/regression/nmag/bigbar/results/nmag4_M.dat')+' '+dirname+' "(oommf)" --oommf'\
                       +' >> '+filename,ignore_error=True)
    
    #if exitcode != 0:
    #    #this could be because the nmag3deviation script didn't exist in earliear versions
    #    #in that case, use most recent version
    #
    #    log.debug("nmag3deviation.py failed, probably because the command didn't exist at the time")
    #    log.debug("nmag3deviation.py failed: nasty hack: will try version in cvs/ocaml/...")
    #    
    #    exitcode = attempt('python '\
    #                       +join('~/cvs/ocaml/tests/regression/nmag/bigbar/results/nmag3deviation.py')\
    #                       +' '\
    #                       +join(dirname,'tests/regression/nmag/bigbar/results/nmag3_M')+' '+dirname+' "(oommf)" --oommf'\
    #                       +' >> '+filename)

    #append empty line to make file easier to read
    attempt('''python -c "print '#'" >>  '''+filename)
        



def send_email(dirname,to,from_):

    #def sendmail( filenames,to,from_,subject,body="",mysmtp = 'localhost'):

    import email, email.Utils, types, os, os.path, mimetypes, string, \
           time, smtplib, logging, exceptions,fcntl,sys,shutil,email.MIMEBase,email.MIMEText

    filenames = [dirname+'_nmag4_M.dat',dirname+'_results.png',\
                 dirname+'_results.eps.gz']

    mysmtp = emailSMTP
    subject = "nmag test results "+dirname

    attempt('cat '+deviationlogfilenmag20070328+' | tail -n 20 > nmag3deviationtail.txt')
    attempt('cat '+deviationlogfileoommf+' | tail -n 20 > oommfdeviationtail.txt')

    body1 = open( dirname+'_info.txt','r').read()
    body2 = open('nmag3deviationtail.txt','r').read()
    body3 = open('oommfdeviationtail.txt','r').read()
    body4 = open('bigbar_timings.log').read()[-3200:]
    
    body = body1+'\n---Deviation to nmag3 ---------------\n'+body2
    body += '\n---Deviation to OOMMF ---------------\n'+body3
    body += '\n---timings ---------------\n'+body4
    body +=   'host 	rev           date	 	        sim 		init 		writing-data 	sim&write 	total'
    attempt('rm -f nmag3deviationtail.txt')
    attempt('rm -f oommfdeviationtail.txt')

    log_global=logging

    """bundle all files given in list of filenames

    This is taken from Matthew Cowles

    http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/86674

    (which guess the file type of the attachements)

    with modification for attaching body from http://snippets.dzone.com/posts/show/757

    Could do with some tidying up (fangohr 27/03/2007)
    """

    if type(to) != types.ListType:
        raise TypeError,"Can't handle type %s for 'to' (needs to be a list)" % type(to)

    import cStringIO
    import base64
    import email.Generator
    import email.Message
    import mimetypes
    import os
    import quopri
    
    mainMsg=email.Message.Message()
    #mainMsg["To"]=' '.join(to)
    from email.Utils import COMMASPACE, formatdate
    mainMsg["To"]=COMMASPACE.join(to)
    mainMsg["From"]=from_
    mainMsg["Subject"]=subject
    mainMsg["Mime-version"]="1.0"
    mainMsg["Content-type"]="Multipart/mixed"
    mainMsg.preamble="Mime message\n"+body
    mainMsg.epilogue="" # To ensure that message ends with newline

    mainMsg.attach(email.MIMEText.MIMEText(body)) # try to attach body
    
    # Get names of plain files
    for fileName in filenames:
        print "working on",fileName
        contentType,ignored=mimetypes.guess_type(fileName)
        if contentType==None: # If no guess, use generic opaque type
            contentType="application/octet-stream"
        contentsEncoded=cStringIO.StringIO()
        f=open(fileName,"rb")
        mainType=contentType[:contentType.find("/")]
        if mainType=="text":
            cte="quoted-printable"
            quopri.encode(f,contentsEncoded,1) # 1 for encode tabs
        else:
            cte="base64"
            base64.encode(f,contentsEncoded)
        f.close()
        subMsg=email.Message.Message()
        subMsg.add_header("Content-type",contentType,name=fileName)
        subMsg.add_header("Content-transfer-encoding",cte)
        subMsg.set_payload(contentsEncoded.getvalue())
        contentsEncoded.close()
        mainMsg.attach(subMsg)

    msg = mainMsg

    fromAddr = from_
    toAddr=to
    
    assert fromAddr, "No from-addr given: %s" % (fromAddr)
    assert toAddr, "No to-addr given: %s" % (toAddr)
    
    log_global.info("sending email from %s to %s" % (fromAddr,toAddr))

    log_global.info("(deliver email to %s via smtp to %s)" % (toAddr,mysmtp))

    import socket

    smtp = smtplib.SMTP(mysmtp)
    smtp.set_debuglevel(0) #put in 1 for messages to stdout/stderr

    smtp.sendmail(fromAddr, to, msg.as_string())
    smtp.quit()
    log_global.debug("finished sending email to %s" % (toAddr))



def copytimings(dirname):
    if os.path.exists('bigbar_timings.log'):
        attempt('''python -c "print '%s'" >> bigbar_timings.log ''' % (dirname))
        attempt('cat '+os.path.join(dirname,'tests/regression/nmag/bigbar/nmag4/bigbar_timings.log')+' | tail -n 1 >> bigbar_timings.log')
    else:
        attempt('cat '+os.path.join(dirname,'tests/regression/nmag/bigbar/nmag4/bigbar_timings.log')+'  > bigbar_timings.log')
    
def make_test(date,emailto,keep=False,headbigbar=False):
    """IF date is provided, it should be of the form (year,month,day,hour,minute))"""

    timings['total'] = -time.time()
    timings['checkout'] = -time.time()
    dirname = checkout(date,headbigbar=headbigbar)
    #dirname="2007-03-27-1134"
    timings['checkout'] += time.time()
    timings['compile'] = -time.time()
    compile(dirname)
    timings['compile'] += time.time()
    timings['do_bigbar'] = -time.time()
    runtest_bigbar(dirname,headbigbar=headbigbar)
    timings['do_bigbar'] += time.time()

    timings['do_deviation'] = -time.time()
    append_deviation_data(dirname)
    copytimings(dirname)
    timings['do_deviation'] += time.time()

    if keep == False:
        timings['remove_files'] = -time.time()
        delete_build_dir(dirname)
        timings['remove_files'] += time.time()

    timings['total'] += time.time()
    create_info_file(dirname,timings)

    if emailto:
        send_email(dirname,emailto,from_=fromAddress)


if __name__ == "__main__":

    usage = "Try --help"
    version = "27 March 2007, $Id$"

    parser = optparse.OptionParser(usage=usage,version=version)

    parser.add_option("--date",help='Date format has to be "(year,month,day,hour,minute)" [including the quotes]',action='store',dest="date")

    parser.add_option("--email",help="comma separated list of recipients",action="store",dest="email")

    parser.add_option("--keep",help="don't delete test directory after having finished",action="store_true",dest="keep")

    parser.add_option("--headbigbar",help="use the most recent code (cvs head) for bigbar.py",action="store_true",dest="headbigbar")

    (options, arguments) = parser.parse_args(sys.argv)

    if options.date:
        date = options.date

        date = eval(options.date)
        if len(date) == 4:
            raise ValueError,"if date is provided, it needs to be formatted like (year,month,day,hour,minute)"
        log.debug("Will checkout and test code for %s" % str(date))
    else:
        date = None
        log.debug("Will checkout and test code (HEAD CVS)")

    if options.email:
        emailto = options.email.split(',')
    else:
        emailto = None

    if options.keep:
        keep = True
    else:
        keep = False

    if options.headbigbar:
        headbigbar = True
    else:
        headbigbar = False

    make_test(date,emailto,keep=keep,headbigbar=headbigbar)

    
