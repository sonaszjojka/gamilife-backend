package edu.pjwstk.groups.usecase.findgroupmemberbyid;

import edu.pjwstk.api.groups.dto.GroupMemberDto;
import edu.pjwstk.groups.model.GroupMember;

public interface GroupMemberMapper {
    GroupMemberDto toResponse(GroupMember groupMember);
}
