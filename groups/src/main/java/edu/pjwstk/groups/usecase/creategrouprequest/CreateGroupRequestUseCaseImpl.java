package edu.pjwstk.groups.usecase.creategrouprequest;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupRequest;
import edu.pjwstk.groups.model.GroupRequestStatus;
import edu.pjwstk.groups.exception.domain.GroupFullException;
import edu.pjwstk.groups.exception.domain.GroupRequestStatusNotFoundException;
import edu.pjwstk.groups.exception.domain.InvalidGroupDataException;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import edu.pjwstk.groups.repository.GroupRequestJpaRepository;
import edu.pjwstk.groups.repository.GroupRequestStatusJpaRepository;
import edu.pjwstk.groups.shared.GroupRequestStatusEnum;
import edu.pjwstk.groups.shared.GroupTypeEnum;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
public class CreateGroupRequestUseCaseImpl implements CreateGroupRequestUseCase {

    private final GroupRequestJpaRepository groupRequestRepository;
    private final GroupJpaRepository groupRepository;
    private final GroupMemberJpaRepository groupMemberRepository;
    private final GroupRequestStatusJpaRepository groupRequestStatusRepository;
    private final AuthApi authApi;
    private final CreateGroupRequestMapper createGroupRequestMapper;

    public CreateGroupRequestUseCaseImpl(GroupRequestJpaRepository groupRequestRepository, GroupJpaRepository groupRepository, GroupMemberJpaRepository groupMemberRepository, GroupRequestStatusJpaRepository groupRequestStatusRepository, AuthApi authApi, CreateGroupRequestMapper createGroupRequestMapper) {
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

        if (groupMemberRepository.existsByUserIdAndMemberGroup(currentUserDto.userId(), group)) {
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

        if (groupRequestRepository.existsByGroupRequestedAndUserIdAndGroupRequestStatus(group, currentUserDto.userId(), groupRequestStatus)) {
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
