/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.binfo;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Map;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

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
            scala.Tuple2<Object,String> result = info.getInfo(request.getParameterMap(), 10);
            out.println(result._2);
            logger.info(getRequestInfo(request, ((Integer)result._1)));
        }                
    }
    
    private String getRequestInfo(HttpServletRequest request,
                                    int total) {
        final Map<String, String[]> parameters = request.getParameterMap();
        final String raddr = request.getRemoteAddr();
        final String referer = request.getHeader("referer");        
        final StringBuilder builder = new StringBuilder();
        
        builder.append("\tdocs_found=" + total);
        
        for (Map.Entry<String, String[]> elem : parameters.entrySet()) {
            final String key = elem.getKey();
            final String[] values = elem.getValue();
            
            for (String value: values) {
                builder.append("\t" + key + "=" + value);
            }
        }        
        
        return "remote_address=" + raddr + "\treferer=" + referer + builder.toString();
    }

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
