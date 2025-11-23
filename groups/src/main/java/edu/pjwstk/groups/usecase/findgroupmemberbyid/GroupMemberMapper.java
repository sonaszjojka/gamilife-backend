package edu.pjwstk.groups.usecase.findgroupmemberbyid;

import edu.pjwstk.api.groups.dto.GroupMemberDto;
import edu.pjwstk.groups.entity.GroupMember;

public interface GroupMemberMapper {
    GroupMemberDto toResponse(GroupMember groupMember);
}
