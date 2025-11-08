package edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest;

import edu.pjwstk.groups.model.GroupRequest;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberResponse;
import org.springframework.stereotype.Component;

@Component
public class EditGroupRequestStatusForGroupRequestMapperImpl implements EditGroupRequestStatusForGroupRequestMapper {

    @Override
    public EditGroupRequestStatusForGroupRequestResponse toResponse(GroupRequest savedGroupRequest,
                                                                    CreateGroupMemberResponse createGroupMemberResponse) {
        if (savedGroupRequest == null) {
            return null;
        }

        EditGroupRequestStatusForGroupRequestResponse.GroupDto groupDto = null;
        if (savedGroupRequest.getGroupRequested() != null) {
            groupDto = new EditGroupRequestStatusForGroupRequestResponse.GroupDto(
                    savedGroupRequest.getGroupRequested().getGroupId()
            );
        }

        EditGroupRequestStatusForGroupRequestResponse.GroupRequestStatusDto statusDto = null;
        if (savedGroupRequest.getGroupRequestStatus() != null) {
            statusDto = new EditGroupRequestStatusForGroupRequestResponse.GroupRequestStatusDto(
                    savedGroupRequest.getGroupRequestStatus().getGroupRequestStatusId(),
                    savedGroupRequest.getGroupRequestStatus().getTitle()
            );
        }

        return EditGroupRequestStatusForGroupRequestResponse.builder()
                .groupRequestId(savedGroupRequest.getGroupRequestId())
                .userId(savedGroupRequest.getUserId())
                .groupRequested(groupDto)
                .createdAt(savedGroupRequest.getCreatedAt())
                .groupRequestStatus(statusDto)
                .groupMember(createGroupMemberResponse)
                .build();
    }
}

