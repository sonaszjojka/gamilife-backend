package edu.pjwstk.groups.usecase.creategrouprequest;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupRequest;
import edu.pjwstk.groups.entity.GroupRequestStatus;
import org.springframework.stereotype.Component;

import java.util.Optional;
import java.util.UUID;

@Component
public class CreateGroupRequestMapperImpl implements CreateGroupRequestMapper {

    @Override
    public GroupRequest toEntity(UUID groupRequestId, Group group, GroupRequestStatus groupRequestStatus, UUID userId) {
        return GroupRequest.builder()
                .groupRequestId(groupRequestId)
                .userId(userId)
                .groupRequested(group)
                .groupRequestStatus(groupRequestStatus)
                .build();
    }

    @Override
    public CreateGroupRequestResponse toResponse(GroupRequest savedGroupRequest) {
        if (savedGroupRequest == null) {
            return null;
        }

        CreateGroupRequestResponse.GroupDto groupDto = null;
        if (savedGroupRequest.getGroupRequested() != null) {
            groupDto = new CreateGroupRequestResponse.GroupDto(
                    savedGroupRequest.getGroupRequested().getGroupId()
            );
        }

        CreateGroupRequestResponse.GroupRequestStatusDto statusDto = null;
        if (savedGroupRequest.getGroupRequestStatus() != null) {
            statusDto = new CreateGroupRequestResponse.GroupRequestStatusDto(
                    savedGroupRequest.getGroupRequestStatus().getGroupRequestStatusId(),
                    savedGroupRequest.getGroupRequestStatus().getTitle()
            );
        }

        return CreateGroupRequestResponse.builder()
                .groupRequestId(savedGroupRequest.getGroupRequestId())
                .userId(savedGroupRequest.getUserId())
                .groupRequested(groupDto)
                .createdAt(savedGroupRequest.getCreatedAt())
                .groupRequestStatus(statusDto)
                .build();
    }

}
