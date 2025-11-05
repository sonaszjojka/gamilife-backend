package edu.pjwstk.groups.usecase.creategrouprequest;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupRequest;
import edu.pjwstk.groups.entity.GroupRequestStatus;
import edu.pjwstk.groups.exception.GroupFullException;
import edu.pjwstk.groups.exception.GroupRequestStatusNotFoundException;
import edu.pjwstk.groups.exception.InvalidGroupDataException;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.groups.repository.GroupRepository;
import edu.pjwstk.groups.repository.GroupRequestRepository;
import edu.pjwstk.groups.repository.GroupRequestStatusRepository;
import edu.pjwstk.groups.shared.GroupRequestStatusEnum;
import edu.pjwstk.groups.shared.GroupTypeEnum;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
public class CreateGroupRequestUseCaseImpl implements CreateGroupRequestUseCase {

    private final GroupRequestRepository groupRequestRepository;
    private final GroupRepository groupRepository;
    private final GroupMemberRepository groupMemberRepository;
    private final GroupRequestStatusRepository groupRequestStatusRepository;
    private final AuthApi authApi;
    private final CreateGroupRequestMapper createGroupRequestMapper;

    public CreateGroupRequestUseCaseImpl(GroupRequestRepository groupRequestRepository, GroupRepository groupRepository, GroupMemberRepository groupMemberRepository, GroupRequestStatusRepository groupRequestStatusRepository, AuthApi authApi, CreateGroupRequestMapper createGroupRequestMapper) {
        this.groupRequestRepository = groupRequestRepository;
        this.groupRepository = groupRepository;
        this.groupMemberRepository = groupMemberRepository;
        this.groupRequestStatusRepository = groupRequestStatusRepository;
        this.authApi = authApi;
        this.createGroupRequestMapper = createGroupRequestMapper;
    }

    @Override
    @Transactional
    public CreateGroupRequestResponse execute(UUID groupId) {
        Group group = groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));

        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (groupMemberRepository.existsByUserIdAndGroup(group, currentUserDto.userId())) {
            throw new InvalidGroupDataException("User with id: " + currentUserDto.userId()
                    + " is already added to group with id:" + groupId);
        }

        if (group.getGroupType().toEnum() == GroupTypeEnum.CLOSED || group.getGroupType().toEnum() == GroupTypeEnum.OPEN) {
            throw new InvalidGroupDataException("This group does not accept join requests. " +
                    "Only 'REQUEST_ONLY' groups can receive join requests.");
        }

        GroupRequestStatus groupRequestStatus = groupRequestStatusRepository.findById(GroupRequestStatusEnum.SENT.getId())
                .orElseThrow(() -> new GroupRequestStatusNotFoundException("Group request status with id: "
                        + GroupRequestStatusEnum.SENT.getId() + " not found!"));

        if (groupRequestRepository.existsByGroupAndUserIdAndGroupRequestStatus(group, currentUserDto.userId(), groupRequestStatus)) {
            throw new InvalidGroupDataException("User with id: " + currentUserDto.userId()
                    + " has already group request with status: SENT to group with id:" + groupId);
        }

        Integer currentMembersCount = groupMemberRepository.countByMemberGroup(group);

        if (currentMembersCount >= group.getMembersLimit()) {
            throw new GroupFullException("Group is full – member limit reached: " + group.getMembersLimit());
        }

        GroupRequest groupRequest = createGroupRequestMapper.toEntity(UUID.randomUUID(), group, groupRequestStatus, currentUserDto.userId());
        GroupRequest savedGroupRequest = groupRequestRepository.save(groupRequest);
        return createGroupRequestMapper.toResponse(savedGroupRequest);
    }
}
