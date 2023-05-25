/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.binfo;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Calendar;
import java.util.Map;

import jakarta.servlet.ServletConfig;
import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

//import org.bireme.aut.IPRange;
import org.bireme.infob.InfoServer;
import org.bireme.infob.MeshConverter;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

/**
 *
 * @author Heitor Barbieri
 * date: 20171215
 */
@WebServlet(name = "BVSInfoButton", urlPatterns = {"/infobutton/search"})
public class BVSInfoButton extends HttpServlet {
    private static final Logger logger = LogManager.getLogger(BVSInfoButton.class);
 
    private InfoServer info;
    private String tpath;
    private MeshConverter mconverter;
    //private IPRange ipRange;

    @Override
    public void init(ServletConfig config) throws ServletException {
        super.init(config);

        final ServletContext context = config.getServletContext();
        final String solrUrl = context.getInitParameter("BVS_SOLR_URL");
        if (solrUrl == null) throw new ServletException(
                                                 "empty 'BVS_SOLR_URL' config");
        tpath = context.getInitParameter("LUCENE_THESAURI_PATH");
        if (tpath == null) throw new ServletException(
                                         "empty 'LUCENE_THESAURI_PATH' config");
        mconverter = new MeshConverter(tpath);
        info = new InfoServer(mconverter, solrUrl);
        
        /*
        final String dbPath = context.getInitParameter("DB_PATH");
        ipRange = (dbPath == null) ? null : new IPRange();
        int x = 0;
        */
    }

    /**
     * Processes requests for both HTTP <code>GET</code> and <code>POST</code>
     * methods.
     *
     * @param request servlet request
     * @param response servlet response
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException if an I/O error occurs
     */
    protected void processRequest(HttpServletRequest request,
                                  HttpServletResponse response)
                                          throws ServletException, IOException {
        final String auxContentType = request.getParameter("knowledgeResponseType");
        final String contentType = (auxContentType == null) ? "text/xml"
                                                 : auxContentType.toLowerCase();
         //sSystem.out.println("processing Request");
         response.setContentType(contentType);
         response.setCharacterEncoding("utf-8");
            
         try (PrintWriter out = response.getWriter()) {
             //out.println(getMainSearchCriteria(request.getParameterMap()));
             //out.println(getDocuments(request.getParameterMap()));
             final scala.Tuple3<Object,String, String[]>
                     result = info.getInfo(request.getParameterMap(), 10);
                          //result = info.getInfo(improveRequest(request).getParameterMap(), 10);
             final String[] ids = result._3();
                            
             out.println(result._2());
             logger.info(getRequestInfo(request, ((Integer)result._1()), ids));
         }                    
    }

    private String getRequestInfo(HttpServletRequest request,
                                  int total,
                                  String[] ids) {
        final Map<String, String[]> parameters = request.getParameterMap();
        final String raddr = request.getRemoteAddr();
        final String referer = request.getHeader("referer");        
        final StringBuilder builder = new StringBuilder();
        final long now = Calendar.getInstance().getTimeInMillis();
        
        builder.append("date=").append(now);
        builder.append("\tremote_address=").append(raddr);
        builder.append("\treferer=").append(referer);
        builder.append("\tdocs_found=").append(total);
        
        boolean first = true;
        builder.append("\tparameters=");
        for (Map.Entry<String, String[]> elem : parameters.entrySet()) {
            final String key = elem.getKey();
            final String[] values = elem.getValue();            
            
            for (String value: values) {
                if (first) {
                    first = false;
                } else {
                    builder.append("|");
                }
                builder.append(key).append("=").append(value);
            }
        }        
        
        first = true;
        builder.append("\tdoc_ids=");
        for (String id: ids) {
            if (first) {
                first = false;
            } else {
                builder.append("|");
            }
            builder.append(id);
        }
        
        return builder.toString();
    }

    /*
    private void improveRequest(final HttpServletRequest request) {
        //See https://stackoverflow.com/questions/29910074/how-to-get-client-ip-address-in-java-httpservletrequest
        String ipAddress = request.getHeader("X-FORWARDED-FOR");
        if (ipAddress == null) {
            ipAddress = request.getRemoteAddr();
        } else {
            ipAddress = ipAddress.contains(",") ? ipAddress.split(",")[0]
                    : ipAddress;
        }
        request.setAttribute("uptodate",
                (ipRange == null) ||
                ipRange.isAuthenticated(ipAddress) ||
                ipRange.isAuthenticated(request.getServerName()));
    }
    */

    // <editor-fold defaultstate="collapsed" desc="HttpServlet methods. Click on the + sign on the left to edit the code.">
    /**
     * Handles the HTTP <code>GET</code> method.
     *
     * @param request servlet request
     * @param response servlet response
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException if an I/O error occurs
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        processRequest(request, response);
    }

    /**
     * Handles the HTTP <code>POST</code> method.
     *
     * @param request servlet request
     * @param response servlet response
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException if an I/O error occurs
     */
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        processRequest(request, response);
    }

    /**
     * Returns a short description of the servlet.
     *
     * @return a String containing servlet description
     */
    @Override
    public String getServletInfo() {
        return "Virtual Health Library Infobutton";
    }// </editor-fold>

}
