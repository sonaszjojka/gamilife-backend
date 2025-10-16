package edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest;

import edu.pjwstk.groups.entity.GroupRequest;
import edu.pjwstk.groups.entity.GroupRequestStatus;
import edu.pjwstk.groups.exception.GroupRequestNotFoundException;
import edu.pjwstk.groups.exception.GroupRequestStatusNotFoundException;
import edu.pjwstk.groups.exception.InvalidGroupRequestDataException;
import edu.pjwstk.groups.repository.GroupRequestRepository;
import edu.pjwstk.groups.repository.GroupRequestStatusRepository;
import edu.pjwstk.groups.shared.GroupRequestStatusEnum;
import jakarta.validation.Valid;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
public class EditGroupRequestStatusForGroupRequestUseCaseImpl implements EditGroupRequestStatusForGroupRequestUseCase {

    private final GroupRequestRepository groupRequestRepository;
    private final GroupRequestStatusRepository groupRequestStatusRepository;
    private final EditGroupRequestStatusForGroupRequestMapper editGroupRequestStatusForGroupRequestMapper;

    public EditGroupRequestStatusForGroupRequestUseCaseImpl(GroupRequestRepository groupRequestRepository, GroupRequestStatusRepository groupRequestStatusRepository, EditGroupRequestStatusForGroupRequestMapper editGroupRequestStatusForGroupRequestMapper) {
        this.groupRequestRepository = groupRequestRepository;
        this.groupRequestStatusRepository = groupRequestStatusRepository;
        this.editGroupRequestStatusForGroupRequestMapper = editGroupRequestStatusForGroupRequestMapper;
    }

    @Override
    @Transactional
    public EditGroupRequestStatusForGroupRequestResponse execute(UUID groupRequestId, EditGroupRequestStatusForGroupRequestRequest request) {
        GroupRequest groupRequest = groupRequestRepository.findById(groupRequestId)
                .orElseThrow(() -> new GroupRequestNotFoundException("Group request with id:" + groupRequestId + " not found!"));

        if (groupRequest.getGroupRequestStatus().toEnum() == GroupRequestStatusEnum.ACCEPTED
                || groupRequest.getGroupRequestStatus().toEnum() == GroupRequestStatusEnum.DECLINED) {
            throw new InvalidGroupRequestDataException("Group requests with status ACCEPTED or DECLINED are final and cannot be changed!");
        }

        GroupRequestStatus groupRequestStatus = groupRequestStatusRepository.findById(request.groupRequestStatus().getId())
                .orElseThrow(() -> new GroupRequestStatusNotFoundException("Group request status with id: "
                        + request.groupRequestStatus().getId() + " not found!"));

        groupRequest.setGroupRequestStatus(groupRequestStatus);

        GroupRequest savedGroupRequest = groupRequestRepository.save(groupRequest);
        return editGroupRequestStatusForGroupRequestMapper.toResponse(savedGroupRequest);
    }
}
