package pl.gamilife.communication.util;

import pl.gamilife.communication.dto.EmailContent;
import pl.gamilife.communication.dto.EmailParameters;

public interface EmailContentCreator {
    EmailContent createContentForParameters(EmailParameters abstractEmailParameters);
}
