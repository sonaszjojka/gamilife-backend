package edu.pjwstk.common.groupsApi;


import edu.pjwstk.common.groupsApi.dto.GroupMemberDto;

public interface GroupApi {
    GroupMemberDto findGroupMemberById(Integer groupMemberId);
}
