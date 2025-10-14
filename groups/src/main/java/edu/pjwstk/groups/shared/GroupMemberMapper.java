package edu.pjwstk.groups.shared;

import edu.pjwstk.common.groupsApi.dto.GroupMemberDto;
import edu.pjwstk.groups.entity.GroupMember;

public interface GroupMemberMapper {
    GroupMemberDto toResponse(GroupMember groupMember);
}
