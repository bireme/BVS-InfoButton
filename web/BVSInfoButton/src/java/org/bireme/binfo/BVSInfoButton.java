/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.binfo;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.List;

import java.util.Map;
import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.bireme.infob.Category;

import org.bireme.infob.InfobuttonServer;
import org.bireme.infob.MeshConverter;
import org.bireme.infob.parameters.AdministrativeGenderCode;
import org.bireme.infob.parameters.Age;
import org.bireme.infob.parameters.AgeGroup;
import org.bireme.infob.parameters.MainSearchCriteria;
import org.bireme.infob.parameters.InfoRecipient;
import org.bireme.infob.parameters.LocationOfInterest;
import org.bireme.infob.parameters.Performer;
import org.bireme.infob.parameters.SubTopic;

import scala.Option;
import scala.Some;
import scala.collection.JavaConverters;
import scala.collection.Seq;

/**
 *
 * @author Heitor Barbieri
 * date: 20171215
 */
@WebServlet(name = "BVSInfoButton", urlPatterns = {"/BVSInfoButton"})
public class BVSInfoButton extends HttpServlet {

    private InfobuttonServer info;
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
        info = new InfobuttonServer(mconverter, solrUrl);

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
        final String contentType = (auxContentType == null) ? "application/json"
                                                 : auxContentType.toLowerCase();
System.out.println("processing Request");
        try (PrintWriter out = response.getWriter()) {
            //out.println(getMainSearchCriteria(request.getParameterMap()));
            //out.println(getDocuments(request.getParameterMap()));
            out.println(info.getInfo(request.getParameterMap(), 10));
        }
    }

    private MainSearchCriteria getMainSearchCriteriaX(Map<String,String[]> params,
                                                      int number) {
        assert number > 1;

        final String[] inputOption = params.get("inputOption" + number);
        final String[] value = params.get("value" + number);
        final String[] codeSystem = params.get("codeSystem" + number);
        final Option<String> sCodeSystem = new Some<>(codeSystem[0]);
        final Option<String> sCode;
        final Option<String> sDisplayName;
        final Option<String> sOriginalText;
        final MainSearchCriteria ret;

        if ((value == null) || (value[0] == null) || (value[0].trim().isEmpty())) {
            ret = null;
        } else {
            if (inputOption[0].equals("code")) {
                sCode = new Some<>(value[0]);
                sDisplayName = Option.apply(null);
                sOriginalText = Option.apply(null);
            } else if (inputOption[0].equals("display_text")) {
                sCode = Option.apply(null);
                sDisplayName = new Some<>(value[0]);
                sOriginalText = Option.apply(null);
            } else {
                sCode = Option.apply(null);
                sDisplayName = Option.apply(null);
                sOriginalText = new Some<>(value[0]);
            }
            final Map<String,String> param = new HashMap<String,String>();

            ret = new MainSearchCriteria(sCode, sCodeSystem, sDisplayName,
                                                                 sOriginalText);
        }
        return ret;
    }

    private AdministrativeGenderCode getAdministrativeGenderCode(Map<String,String[]> params) {
      final AdministrativeGenderCode agc = getAdministrativeGenderCodeCC(params);

      return (agc == null) ? getAdministrativeGenderCodeDN(params) : agc;
    }

    private AdministrativeGenderCode getAdministrativeGenderCodeCC(Map<String,String[]> params) {
        final String[] admGenderCode = params.get("admGenderCode_conceptCode");
        final AdministrativeGenderCode ret;

        if (admGenderCode == null) {
            ret = null;
        } else {
            final Option<String> code;
            if (admGenderCode[0].equals("---do not use---")) {
                code = Option.apply(null);
            } else if (admGenderCode[0].equals("UN")) {
                code = Option.apply(null);
            } else if (admGenderCode[0].equals("M")) {
                code = new Some<>("M");
            } else if (admGenderCode[0].equals("F")) {
                code = new Some<>("F");
            } else {
                code = Option.apply(null);
            }
            ret = code.isEmpty() ? null :
                           new AdministrativeGenderCode(code, Option.apply(null));
        }
        return ret;
    }

    private AdministrativeGenderCode getAdministrativeGenderCodeDN(Map<String,String[]> params) {
        final String[] admGenderCode = params.get("admGenderCode_displayName");
        final AdministrativeGenderCode ret;

        if (admGenderCode == null) {
            ret = null;
        } else {
            final Option<String> code;
            if (admGenderCode[0].equals("---do not use---")) {
                code = Option.apply(null);
            } else if (admGenderCode[0].equals("undifferentiated")) {
                code = Option.apply(null);
            } else if (admGenderCode[0].equals("male")) {
                code = new Some<>("male");
            } else if (admGenderCode[0].equals("female")) {
                code = new Some<>("female");
            } else {
                code = Option.apply(null);
            }
            ret = code.isEmpty() ? null :
                           new AdministrativeGenderCode(Option.apply(null),code);
        }
        return ret;
    }

    private AgeGroup getAgeGroup(Map<String,String[]> params) {
        final String[] ageGroup = params.get("ageGroup");
        final AgeGroup ret;

        if (ageGroup == null) {
            ret = null;
        } else {
            final Option<String> code;
            if (ageGroup[0].equals("---do not use---")) {
                code = Option.apply(null);
            } else if (ageGroup[0].equals("infant, newborn")) {
                code = new Some<>("infant, newborn");
            } else if (ageGroup[0].equals("infant")) {
                code = new Some<>("infant");
            } else if (ageGroup[0].equals("child, preschool")) {
                code = new Some<>("child, preschool");
            } else if (ageGroup[0].equals("child")) {
                code = new Some<>("child");
            } else if (ageGroup[0].equals("adolescent")) {
                code = new Some<>("adolescent");
            } else if (ageGroup[0].equals("young adult")) {
                code = new Some<>("young adult");
            } else if (ageGroup[0].equals("adult")) {
                code = new Some<>("adult");
            } else if (ageGroup[0].equals("middle aged")) {
                code = new Some<>("middle aged");
            } else if (ageGroup[0].equals("aged")) {
                code = new Some<>("aged");
            } else if (ageGroup[0].equals("aged, 80 and older")) {
                code = new Some<>("aged, 80 and older");
            } else {
                code = Option.apply(null);
            }
            ret = code.isEmpty() ? null :
                        new AgeGroup(Option.apply(null),Option.apply(null), code);
        }
        return ret;
    }

    private Age getAge(Map<String,String[]> params) {
        final String[] age = params.get("age");
        final Age ret;

        if ((age == null) || (age[0].equals("do_not_use"))) {
            ret = null;
        } else {
            final Option<String> code;
            final String[]split = age[0].split(" ");
            final String value;
            final String unit;

            if (age[0].equals("---do not use---")) {
                value = null;
                unit = null;
            } else {
                value = split[0];
                unit = split[1];
            }
            ret = (value == null) ? null
                                 : new Age(new Some<>(value), new Some<>(unit));
        }
        return ret;
    }

    private InfoRecipient getInfoRecipient(Map<String,String[]> params) {
        final String[] infoRecipientRole = params.get("infoRecipientRole");
        final String[] infoRecipientLanguage = params.get("infoRecipientLanguage");
        final InfoRecipient ret;

        if ((infoRecipientRole == null) || (infoRecipientLanguage == null)) {
            ret = null;
        } else {
            final Option<String> infoRole;
            if (infoRecipientRole[0].equals("---do not use---")) {
                infoRole = Option.apply(null);
            } else if (infoRecipientRole[0].equals("PAT")) {
                infoRole = new Some<>("PAT");
            } else if (infoRecipientRole[0].equals("PROV")) {
                infoRole = new Some<>("PROV");
            } else if (infoRecipientRole[0].equals("PAYOR")) {
                infoRole = new Some<>("PAYOR");
            } else {
                infoRole = Option.apply(null);
            }
            final Option<String> infoLang;
            if (infoRecipientLanguage[0].equals("---do not use---")) {
                infoLang = Option.apply(null);
            } else if (infoRecipientLanguage[0].equals("english")) {
                infoLang = new Some<>("english");
            } else if (infoRecipientLanguage[0].equals("spanish")) {
                infoLang = new Some<>("spanish");
            } else if (infoRecipientLanguage[0].equals("portuguese")) {
                infoLang = new Some<>("portuguese");
            } else if (infoRecipientLanguage[0].equals("french")) {
                infoLang = new Some<>("french");
            } else if (infoRecipientLanguage[0].equals("chinese")) {
                infoLang = new Some<>("chinese");
            } else if (infoRecipientLanguage[0].equals("german")) {
                infoLang = new Some<>("german");
            } else if (infoRecipientLanguage[0].equals("russian")) {
                infoLang = new Some<>("russian");
            } else if (infoRecipientLanguage[0].equals("japanese")) {
                infoLang = new Some<>("japanese");
            } else if (infoRecipientLanguage[0].equals("dutch")) {
                infoLang = new Some<>("dutch");
            } else if (infoRecipientLanguage[0].equals("arabic")) {
                infoLang = new Some<>("arabic");
            } else if (infoRecipientLanguage[0].equals("polish")) {
                infoLang = new Some<>("polish");
            } else if (infoRecipientLanguage[0].equals("danish")) {
                infoLang = new Some<>("danish");
            } else if (infoRecipientLanguage[0].equals("italian")) {
                infoLang = new Some<>("italian");
            } else if (infoRecipientLanguage[0].equals("norwegian")) {
                infoLang = new Some<>("norwegian");
            } else {
                infoLang = Option.apply(null);
            }
            ret = new InfoRecipient(infoRole, Option.apply(null),Option.apply(null),
                                 Option.apply(null),Option.apply(null), 
                                 Option.apply(null), infoLang);
        }
        return ret;
    }

    private Performer getPerformer(Map<String,String[]> params) {
        final String[] performerRole = params.get("performerRole");
        final String[] performerLanguage = params.get("performerLanguage");
        final Performer ret;

        if ((performerRole == null) || (performerLanguage == null)) {
            ret = null;
        } else {
            final Option<String> perfRole;
            if (performerRole[0].equals("---do not use---")) {
                perfRole = Option.apply(null);
            } else if (performerRole[0].equals("PAT")) {
                perfRole = new Some<>("PAT");
            } else if (performerRole[0].equals("PROV")) {
                perfRole = new Some<>("PROV");
            } else if (performerRole[0].equals("PAYOR")) {
                perfRole = new Some<>("PAYOR");
            } else {
                perfRole = Option.apply(null);
            }
            final Option<String> performerLang;
            if (performerLanguage[0].equals("---do not use---")) {
                performerLang = Option.apply(null);
            } else if (performerLanguage[0].equals("english")) {
                performerLang = new Some<>("english");
            } else if (performerLanguage[0].equals("spanish")) {
                performerLang = new Some<>("spanish");
            } else if (performerLanguage[0].equals("portuguese")) {
                performerLang = new Some<>("portuguese");
            } else if (performerLanguage[0].equals("french")) {
                performerLang = new Some<>("french");
            } else if (performerLanguage[0].equals("chinese")) {
                performerLang = new Some<>("chinese");
            } else if (performerLanguage[0].equals("german")) {
                performerLang = new Some<>("german");
            } else if (performerLanguage[0].equals("russian")) {
                performerLang = new Some<>("russian");
            } else if (performerLanguage[0].equals("japanese")) {
                performerLang = new Some<>("japanese");
            } else if (performerLanguage[0].equals("dutch")) {
                performerLang = new Some<>("dutch");
            } else if (performerLanguage[0].equals("arabic")) {
                performerLang = new Some<>("arabic");
            } else if (performerLanguage[0].equals("polish")) {
                performerLang = new Some<>("polish");
            } else if (performerLanguage[0].equals("danish")) {
                performerLang = new Some<>("danish");
            } else if (performerLanguage[0].equals("italian")) {
                performerLang = new Some<>("italian");
            } else if (performerLanguage[0].equals("norwegian")) {
                performerLang = new Some<>("norwegian");
            } else {
                performerLang = Option.apply(null);
            }
            ret = new Performer(perfRole, Option.apply(null),
                                    Option.apply(null), performerLang);
        }
        return ret;
    }

    private SubTopic getSubTopic(Map<String,String[]> params) {
        final String[] subTopic = params.get("subTopic");
        final SubTopic ret;

        if (subTopic == null) {
            ret = null;
        } else {
            final String opt = subTopic[0];
            if (opt.equals("---do not use---")) {
                ret = null;
            } else {
                final Option<String> codeSystem = new Some<>(
                    (Character.isLetter(opt.charAt(0))) ?
                    "2.16.840.1.113883.6.177" : "2.16.840.1.113883.6.96");
                final Option<String> code = new Some<>(opt);
                ret = opt.isEmpty() ? null :
                    new SubTopic(codeSystem, code, Option.apply(null),
                                                             Option.apply(null));
            }
        }
        return ret;
    }

    private LocationOfInterest getLocationOfInterestX(Map<String,String[]> params,
                                                      int number) {
        final String[] loi = params.get("locationOfInterest" + number);
        final LocationOfInterest ret;

        if (loi == null) {
            ret = null;
        } else {
            final String opt = loi[0];
            if (opt.equals("---do not use---")) {
                ret = null;
            } else {
                final Option<String> codeSystem = new Some<>("2.16.840.1.113883.5.16");
                final Option<String> conceptCode = new Some<>("CNT");
                final Option<String> code = new Some<>(opt);
                ret = opt.isEmpty() ? null :
                    new LocationOfInterest(codeSystem, conceptCode, code,
                                                             Option.apply(null));
System.out.println("LOI => " + ret.toString());
            }
        }
        return ret;
    }

   /* private String getDocuments(Map<String,String[]> params) {
        final Map<String,String> param = new HashMap<>();
        final MainSearchCriteria msc1 = getMainSearchCriteriaX(params, 1);
        final MainSearchCriteria msc2 = getMainSearchCriteriaX(params, 2);
        final boolean both = (msc1 != null) && (msc2 != null);

        if (both) {
            addCategories(msc1.getCategories(0), param);
            addCategories(msc2.getCategories(1), param);
        } else {
            if (msc1 != null) {
                addCategories(msc1.getCategories(), param);
            }
            if (msc2 != null) {
                addCategories(msc2.getCategories(), param);
            }
        }
        final AdministrativeGenderCode adg = getAdministrativeGenderCode(params);
        if (adg != null) {
            addCategories(adg.getCategories(), param);
        }
        final AgeGroup ag = getAgeGroup(params);
        if (ag != null) {
            addCategories(ag.getCategories(), param);
        }
        final Age age = getAge(params);
        if (age != null) {
            addCategories(age.getCategories(), param);
        }
        final InfoRecipient ir = getInfoRecipient(params);
        if (ir != null) {
            addCategories(ir.getCategories(), param);
        }
        final Performer perf = getPerformer(params);
        if (perf != null) {
            addCategories(perf.getCategories(), param);
        }
        final SubTopic sub = getSubTopic(params);
        if (sub != null) {
            addCategories(sub.getCategories(), param);
        }
        final LocationOfInterest loi1 = getLocationOfInterestX(params, 1);
        if (loi1 != null) {
            addCategories(loi1.getCategories(), param);
        }
        final LocationOfInterest loi2 = getLocationOfInterestX(params, 2);
        if (loi2 != null) {
            addCategories(loi2.getCategories(), param);
        }
//for (Map.Entry<String,String> entry : param.entrySet()) {
//    System.out.println("key=" + entry.getKey() + " value=" + entry.getValue());
//}
        return info.getInfo(param, 10);
    }
*/
    
    private void addCategories(final Seq<Category> categories,
                               final Map<String,String> param) {
        final List<Category> categ = JavaConverters.seqAsJavaList(categories);

        for (Category cat:categ) {
            if (!cat.term().isEmpty() && !cat.term().equals("do_not_use")) {
System.out.println("Adcionando categoria key=" + cat.scheme() + " value=" + cat.term());
                param.put(cat.scheme(), cat.term());
            }
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
