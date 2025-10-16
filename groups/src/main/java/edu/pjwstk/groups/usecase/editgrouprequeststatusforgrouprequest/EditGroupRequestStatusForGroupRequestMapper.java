package edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest;

import edu.pjwstk.groups.entity.GroupRequest;

public interface EditGroupRequestStatusForGroupRequestMapper {
    EditGroupRequestStatusForGroupRequestResponse toResponse(GroupRequest savedGroupRequest);
}
