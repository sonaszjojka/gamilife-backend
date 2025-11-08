package edu.pjwstk.groups.usecase.creategroupmember;

import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.exception.domain.GroupFullException;
import edu.pjwstk.groups.exception.domain.UserAlreadyMemberOfGroupException;
import edu.pjwstk.groups.exception.domain.UserJoinGroupAccessDeniedException;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import edu.pjwstk.groups.shared.GroupTypeEnum;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.UUID;

@Service
public class CreateGroupMemberInOpenGroupUseCaseImpl implements CreateGroupMemberInOpenGroupUseCase {

    private final GroupMemberJpaRepository groupMemberRepository;
    private final GroupJpaRepository groupRepository;
    private final UserApi userApi;
    private final CreateGroupMemberMapper createGroupMemberMapper;

    public CreateGroupMemberInOpenGroupUseCaseImpl(GroupMemberJpaRepository groupMemberRepository, GroupJpaRepository groupRepository,
                                                   UserApi userApi, CreateGroupMemberMapper createGroupMemberMapper) {
        this.groupMemberRepository = groupMemberRepository;
        this.groupRepository = groupRepository;
        this.userApi = userApi;
        this.createGroupMemberMapper = createGroupMemberMapper;
    }

    @Override
    @Transactional
    public CreateGroupMemberResponse execute(CreateGroupMemberRequest request, UUID groupId) {
        Group group = groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));

        BasicUserInfoApiDto userInfoApiDto = userApi.getUserById(request.userId())
                .orElseThrow(() -> new UserNotFoundException("User with id: " + request.userId() + " not found!"));

        if (group.getGroupMembers().size() >= group.getMembersLimit()) {
            throw new GroupFullException("Group with id: " + groupId + " is full!");
        }

        if (group.getGroupType().toEnum() == GroupTypeEnum.CLOSED
                || group.getGroupType().toEnum() == GroupTypeEnum.REQUEST_ONLY) {
            throw new UserJoinGroupAccessDeniedException("To add user to group which type is: REQUEST_ONLY or CLOSED " +
                    " - invitation or request must be accepted.");
        }

        Optional<GroupMember> groupMemberOpt = groupMemberRepository.findByUserIdAndMemberGroup(userInfoApiDto.userId(), group);

        if (groupMemberOpt.isPresent()) {
            GroupMember groupMember = groupMemberOpt.get();

            if (groupMember.getLeftAt() != null) {
                groupMember.setLeftAt(null);
                GroupMember existingGroupMember = groupMemberRepository.save(groupMember);
                return createGroupMemberMapper.toResponse(existingGroupMember);
            } else {
                throw new UserAlreadyMemberOfGroupException(
                        "User with id: " + userInfoApiDto.userId() + " is already member of group with id: " + groupId
                );
            }
        }

        GroupMember newGroupMember = createGroupMemberMapper.toEntity(userInfoApiDto.userId(), group, UUID.randomUUID());
        GroupMember savedGroupMember = groupMemberRepository.save(newGroupMember);
        return createGroupMemberMapper.toResponse(savedGroupMember);

    }
}
