package edu.pjwstk.groups.usecase.editgroupmember;

import edu.pjwstk.groups.model.GroupMember;

public interface EditGroupMemberMapper {

    EditGroupMemberResponse toResponse(GroupMember savedGroupMember);
}
