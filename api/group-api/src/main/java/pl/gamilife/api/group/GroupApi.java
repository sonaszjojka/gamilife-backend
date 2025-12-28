package pl.gamilife.api.group;


import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.api.group.dto.GroupMemberDto;

import java.time.ZoneId;
import java.util.UUID;

public interface GroupApi {

    ZoneId getGroupTimezone(UUID groupId);

    GroupMemberDto findGroupMemberById(UUID groupMemberId);

    GroupDto findGroupById(UUID groupId);

    void editMemberWallet(UUID memberId, UUID groupId, Integer amount);
}
