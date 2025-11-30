package edu.pjwstk.communication.util;

import edu.pjwstk.communication.dto.EmailContent;
import edu.pjwstk.communication.dto.EmailParameters;

public interface EmailContentCreator {
    EmailContent createContentForParameters(EmailParameters abstractEmailParameters);
}
