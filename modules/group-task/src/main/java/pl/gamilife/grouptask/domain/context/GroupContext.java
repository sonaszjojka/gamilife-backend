package pl.gamilife.grouptask.domain.context;

import java.time.LocalDateTime;
import java.util.UUID;

public interface GroupContext {
    LocalDateTime getCurrentGroupDateTime(UUID groupId);
}
