package edu.pjwstk.groups.usecase.leavegroup;

import edu.pjwstk.groups.entity.GroupMember;

public interface LeaveGroupMapper {

    LeaveGroupResponse toResponse(GroupMember savedGroupMember);
}
