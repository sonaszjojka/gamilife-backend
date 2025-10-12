package edu.pjwstk.groups.api;

import edu.pjwstk.groups.shared.GroupMemberDto;

public interface GroupApi {
    GroupMemberDto findGroupMemberById(Integer groupMemberId);
}
