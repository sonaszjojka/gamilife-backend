package edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.groups.model.GroupRequest;
import edu.pjwstk.groups.model.GroupRequestStatus;
import edu.pjwstk.groups.exception.domain.GroupRequestNotFoundException;
import edu.pjwstk.groups.exception.domain.GroupRequestStatusNotFoundException;
import edu.pjwstk.groups.exception.domain.InvalidGroupDataException;
import edu.pjwstk.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import edu.pjwstk.groups.repository.GroupRequestJpaRepository;
import edu.pjwstk.groups.repository.GroupRequestStatusJpaRepository;
import edu.pjwstk.groups.shared.GroupRequestStatusEnum;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberResponse;
import edu.pjwstk.groups.usecase.creategroupmemberafteracceptation.CreateGroupMemberAfterAcceptationRequest;
import edu.pjwstk.groups.usecase.creategroupmemberafteracceptation.CreateGroupMemberAfterAcceptationUseCase;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;
import java.util.UUID;

@Service
public class EditGroupRequestStatusForGroupRequestUseCaseImpl implements EditGroupRequestStatusForGroupRequestUseCase {

    private final GroupRequestJpaRepository groupRequestRepository;
    private final GroupRequestStatusJpaRepository groupRequestStatusRepository;
    private final EditGroupRequestStatusForGroupRequestMapper editGroupRequestStatusForGroupRequestMapper;
    private final AuthApi authApi;
    private final CreateGroupMemberAfterAcceptationUseCase createGroupMemberAfterAcceptationUseCase;

    public EditGroupRequestStatusForGroupRequestUseCaseImpl(GroupRequestJpaRepository groupRequestRepository, GroupRequestStatusJpaRepository groupRequestStatusRepository, EditGroupRequestStatusForGroupRequestMapper editGroupRequestStatusForGroupRequestMapper, AuthApi authApi, CreateGroupMemberAfterAcceptationUseCase createGroupMemberAfterAcceptationUseCase) {
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

        GroupRequestStatus groupRequestStatus = groupRequestStatusRepository.findById(request.groupRequestStatusId())
                .orElseThrow(() -> new GroupRequestStatusNotFoundException("Group request status with id: "
                        + request.groupRequestStatusId() + " not found!"));

        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (!Objects.equals(currentUserDto.userId(), groupRequest.getGroupRequested().getAdminId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can change status group request!");
        }

        if (groupRequest.getGroupRequestStatus().toEnum() == GroupRequestStatusEnum.ACCEPTED
                || groupRequest.getGroupRequestStatus().toEnum() == GroupRequestStatusEnum.DECLINED) {
            throw new InvalidGroupDataException("Group requests with status ACCEPTED or DECLINED are final and cannot be changed!");
        }

        if (groupRequestStatus.toEnum() == GroupRequestStatusEnum.SENT) {
            throw new InvalidGroupDataException("Group requests with id: " + groupRequestId + " has already status: SENT");
        }

        CreateGroupMemberResponse createGroupMemberResponse = null;
        if (groupRequestStatus.toEnum()  == GroupRequestStatusEnum.ACCEPTED) {
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
