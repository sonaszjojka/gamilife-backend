package edu.pjwstk.groups.usecase.leavegroup;

import edu.pjwstk.groups.model.GroupMember;

public interface LeaveGroupMapper {

    LeaveGroupResponse toResponse(GroupMember savedGroupMember);
}
