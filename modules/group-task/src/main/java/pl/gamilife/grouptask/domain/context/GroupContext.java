package pl.gamilife.grouptask.domain.context;

import pl.gamilife.api.group.dto.BasicGroupMemberDto;

import java.time.ZoneId;
import java.util.Collection;
import java.util.UUID;

public interface GroupContext {
    ZoneId getCurrentGroupTimezone(UUID groupId);

    Collection<BasicGroupMemberDto> findMembersByIdIn(Collection<UUID> groupMemberIds);
}
