package edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.groups.entity.GroupRequest;
import edu.pjwstk.groups.entity.GroupRequestStatus;
import edu.pjwstk.groups.exception.GroupRequestNotFoundException;
import edu.pjwstk.groups.exception.GroupRequestStatusNotFoundException;
import edu.pjwstk.groups.exception.InvalidGroupDataException;
import edu.pjwstk.groups.exception.UserNotGroupAdministratorAccessDeniedException;
import edu.pjwstk.groups.repository.GroupRequestRepository;
import edu.pjwstk.groups.repository.GroupRequestStatusRepository;
import edu.pjwstk.groups.shared.GroupRequestStatusEnum;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
public class EditGroupRequestStatusForGroupRequestUseCaseImpl implements EditGroupRequestStatusForGroupRequestUseCase {

    private final GroupRequestRepository groupRequestRepository;
    private final GroupRequestStatusRepository groupRequestStatusRepository;
    private final EditGroupRequestStatusForGroupRequestMapper editGroupRequestStatusForGroupRequestMapper;
    private final AuthApi authApi;

    public EditGroupRequestStatusForGroupRequestUseCaseImpl(GroupRequestRepository groupRequestRepository, GroupRequestStatusRepository groupRequestStatusRepository, EditGroupRequestStatusForGroupRequestMapper editGroupRequestStatusForGroupRequestMapper, AuthApi authApi) {
        this.groupRequestRepository = groupRequestRepository;
        this.groupRequestStatusRepository = groupRequestStatusRepository;
        this.editGroupRequestStatusForGroupRequestMapper = editGroupRequestStatusForGroupRequestMapper;
        this.authApi = authApi;
    }

    @Override
    @Transactional
    public EditGroupRequestStatusForGroupRequestResponse execute(UUID groupRequestId, EditGroupRequestStatusForGroupRequestRequest request) {
        GroupRequest groupRequest = groupRequestRepository.findById(groupRequestId)
                .orElseThrow(() -> new GroupRequestNotFoundException("Group request with id:" + groupRequestId + " not found!"));

        CurrentUserDto currentUserDto = authApi.getCurrentUser()
                .orElseThrow();

        if (currentUserDto.userId() != groupRequest.getGroupRequested().getAdminId()) {
            throw new UserNotGroupAdministratorAccessDeniedException("Only group administrators can change status group request!");
        }

        if (groupRequest.getGroupRequestStatus().toEnum() == GroupRequestStatusEnum.ACCEPTED
                || groupRequest.getGroupRequestStatus().toEnum() == GroupRequestStatusEnum.DECLINED) {
            throw new InvalidGroupDataException("Group requests with status ACCEPTED or DECLINED are final and cannot be changed!");
        }

        GroupRequestStatus groupRequestStatus = groupRequestStatusRepository.findById(request.groupRequestStatus().getId())
                .orElseThrow(() -> new GroupRequestStatusNotFoundException("Group request status with id: "
                        + request.groupRequestStatus().getId() + " not found!"));

        groupRequest.setGroupRequestStatus(groupRequestStatus);

        GroupRequest savedGroupRequest = groupRequestRepository.save(groupRequest);
        return editGroupRequestStatusForGroupRequestMapper.toResponse(savedGroupRequest);
    }
}
