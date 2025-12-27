package pl.gamilife.grouptask.domain.context;

import java.time.ZoneId;
import java.util.UUID;

public interface GroupContext {
    ZoneId getCurrentGroupTimezone(UUID groupId);
}
