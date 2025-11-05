package edu.pjwstk.api.groups;


import edu.pjwstk.api.groups.dto.GroupDto;
import edu.pjwstk.api.groups.dto.GroupMemberDto;

import java.util.UUID;

public interface GroupApi {
    GroupMemberDto findGroupMemberById(UUID groupMemberId);
    GroupDto findGroupById(UUID groupId);
}
