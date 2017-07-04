/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.bib;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.bireme.infob.InfobuttonServer;
import org.bireme.infob.MeshConverter;

/**
 *
 * @author Heitor Barbieri
 */
@WebServlet(name = "BVSInfoButtonServlet", urlPatterns = {"/BVSInfoButtonServlet"})
public class BVSInfoButtonServlet extends HttpServlet {
    final InfobuttonServer info = new InfobuttonServer(new MeshConverter(),
                         "http://basalto02.bireme.br:8986/solr5/portal/select");
    
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
        final String contentType = (auxContentType == null) ? "application/json"
                                                 : auxContentType.toLowerCase();
        final Map<String,String> params = new HashMap<>();
        request.getParameterMap().entrySet().forEach((entry) -> {
            params.put(entry.getKey(), entry.getValue()[0]);
        });
        
        response.setContentType(contentType + ";charset=UTF-8");
        
        try (PrintWriter out = response.getWriter()) {
            out.println(info.getInfo(params, 20));
        }
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
