package edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest;


import java.util.UUID;

public interface EditGroupRequestStatusForGroupRequestUseCase {
    EditGroupRequestStatusForGroupRequestResponse execute(UUID groupRequestId, EditGroupRequestStatusForGroupRequestRequest request);
}
