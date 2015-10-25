/* (C) 2000-2002, DIUF, http://www.unifr.ch/diuf
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
 * Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

package iiuf.xmillum.handlers;

import iiuf.dom.DOMUtils;
import iiuf.xmillum.ActionHandler;
import iiuf.xmillum.ActionHandlerParam;
import iiuf.xmillum.BrowserContext;
import java.util.Iterator;
import java.util.Set;
import org.w3c.dom.Element;

/**
 * Output
 *
 * ActionHandler that prints the element in question on StdOut.
 *
 * @author $Author: ohitz $
 * @version $Revision: 1.1 $
 */
public class Output extends ActionHandler {
  public void init(BrowserContext c, Element e) {
    System.out.println(Output.class.getName()+" initialized");
  }

  public void handle(ActionHandlerParam param) {
    Element source = param.getContext().getSourceElementByReference(param.getElement().getAttribute("ref"));
    System.out.println(DOMUtils.toString(source));
  }
}
