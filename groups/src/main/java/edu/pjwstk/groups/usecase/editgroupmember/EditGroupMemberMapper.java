package edu.pjwstk.groups.usecase.editgroupmember;

import edu.pjwstk.groups.entity.GroupMember;

public interface EditGroupMemberMapper {

    EditGroupMemberResponse toResponse(GroupMember savedGroupMember);
}
