package edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest;

import edu.pjwstk.groups.entity.GroupRequest;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberResponse;

public interface EditGroupRequestStatusForGroupRequestMapper {
    EditGroupRequestStatusForGroupRequestResponse toResponse(GroupRequest savedGroupRequest,
                                                             CreateGroupMemberResponse createGroupMemberResponse);
}
