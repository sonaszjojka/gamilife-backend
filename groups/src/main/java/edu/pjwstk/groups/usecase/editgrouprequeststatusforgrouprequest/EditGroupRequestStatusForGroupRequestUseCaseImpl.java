package edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.groups.entity.GroupRequest;
import edu.pjwstk.groups.entity.GroupRequestStatus;
import edu.pjwstk.groups.exception.GroupRequestNotFoundException;
import edu.pjwstk.groups.exception.GroupRequestStatusNotFoundException;
import edu.pjwstk.groups.exception.InvalidGroupDataException;
import edu.pjwstk.groups.exception.UserNotGroupAdministratorAccessDeniedException;
import edu.pjwstk.groups.repository.GroupRequestRepository;
import edu.pjwstk.groups.repository.GroupRequestStatusRepository;
import edu.pjwstk.groups.shared.GroupRequestStatusEnum;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberResponse;
import edu.pjwstk.groups.usecase.creategroupmember.creategroupmemberafteracceptation.CreateGroupMemberAfterAcceptationRequest;
import edu.pjwstk.groups.usecase.creategroupmember.creategroupmemberafteracceptation.CreateGroupMemberAfterAcceptationUseCase;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;
import java.util.UUID;

@Service
public class EditGroupRequestStatusForGroupRequestUseCaseImpl implements EditGroupRequestStatusForGroupRequestUseCase {

    private final GroupRequestRepository groupRequestRepository;
    private final GroupRequestStatusRepository groupRequestStatusRepository;
    private final EditGroupRequestStatusForGroupRequestMapper editGroupRequestStatusForGroupRequestMapper;
    private final AuthApi authApi;
    private final CreateGroupMemberAfterAcceptationUseCase createGroupMemberAfterAcceptationUseCase;

    public EditGroupRequestStatusForGroupRequestUseCaseImpl(GroupRequestRepository groupRequestRepository, GroupRequestStatusRepository groupRequestStatusRepository, EditGroupRequestStatusForGroupRequestMapper editGroupRequestStatusForGroupRequestMapper, AuthApi authApi, CreateGroupMemberAfterAcceptationUseCase createGroupMemberAfterAcceptationUseCase) {
        this.groupRequestRepository = groupRequestRepository;
        this.groupRequestStatusRepository = groupRequestStatusRepository;
        this.editGroupRequestStatusForGroupRequestMapper = editGroupRequestStatusForGroupRequestMapper;
        this.authApi = authApi;
        this.createGroupMemberAfterAcceptationUseCase = createGroupMemberAfterAcceptationUseCase;
    }

    @Override
    @Transactional
    public EditGroupRequestStatusForGroupRequestResponse execute(UUID groupRequestId, EditGroupRequestStatusForGroupRequestRequest request) {
        GroupRequest groupRequest = groupRequestRepository.findById(groupRequestId)
                .orElseThrow(() -> new GroupRequestNotFoundException("Group request with id:" + groupRequestId + " not found!"));

        CurrentUserDto currentUserDto = authApi.getCurrentUser()
                .orElseThrow();

        if (!Objects.equals(currentUserDto.userId(), groupRequest.getGroupRequested().getAdminId())) {
            throw new UserNotGroupAdministratorAccessDeniedException("Only group administrators can change status group request!");
        }

        if (groupRequest.getGroupRequestStatus().toEnum() == GroupRequestStatusEnum.ACCEPTED
                || groupRequest.getGroupRequestStatus().toEnum() == GroupRequestStatusEnum.DECLINED) {
            throw new InvalidGroupDataException("Group requests with status ACCEPTED or DECLINED are final and cannot be changed!");
        }

        if (request.groupRequestStatus() == GroupRequestStatusEnum.SENT) {
            throw new InvalidGroupDataException("Group requests with id: " + groupRequestId + " has already status: SENT");
        }

        GroupRequestStatus groupRequestStatus = groupRequestStatusRepository.findById(request.groupRequestStatus().getId())
                .orElseThrow(() -> new GroupRequestStatusNotFoundException("Group request status with id: "
                        + request.groupRequestStatus().getId() + " not found!"));

        CreateGroupMemberResponse createGroupMemberResponse = null;
        if (request.groupRequestStatus() == GroupRequestStatusEnum.ACCEPTED) {
            createGroupMemberResponse = createGroupMemberAfterAcceptationUseCase.execute(
                    CreateGroupMemberAfterAcceptationRequest.builder()
                            .groupId(groupRequest.getGroupRequested().getGroupId())
                            .userId(groupRequest.getUserId())
                            .build());
        }

        groupRequest.setGroupRequestStatus(groupRequestStatus);
        GroupRequest savedGroupRequest = groupRequestRepository.save(groupRequest);
        return editGroupRequestStatusForGroupRequestMapper.toResponse(savedGroupRequest, createGroupMemberResponse);
    }
}
