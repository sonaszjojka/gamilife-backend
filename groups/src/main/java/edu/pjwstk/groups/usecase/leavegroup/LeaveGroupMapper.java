package edu.pjwstk.groups.usecase.leavegroup;

import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.usecase.editgroupmember.EditGroupMemberResponse;

public interface LeaveGroupMapper {

    LeaveGroupResponse toResponse(GroupMember savedGroupMember);
}
