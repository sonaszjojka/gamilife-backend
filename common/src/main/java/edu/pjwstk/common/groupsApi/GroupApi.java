package edu.pjwstk.common.groupsApi;


import edu.pjwstk.common.groupsApi.dto.GroupMemberDto;

import java.util.UUID;

public interface GroupApi {
    GroupMemberDto findGroupMemberById(UUID groupMemberId);
}
