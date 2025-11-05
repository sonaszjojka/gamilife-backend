package edu.pjwstk.groups.usecase.creategroupmember;

import edu.pjwstk.api.groups.exception.GroupNotFoundException;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.api.user.exception.UserNotFoundException;
import edu.pjwstk.groups.entity.*;
import edu.pjwstk.groups.exception.*;
import edu.pjwstk.groups.repository.*;
import edu.pjwstk.groups.shared.GroupTypeEnum;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.UUID;

@Service
public class CreateGroupMemberInOpenGroupUseCaseImpl implements CreateGroupMemberInOpenGroupUseCase {

    private final GroupMemberRepository groupMemberRepository;
    private final GroupRepository groupRepository;
    private final UserApi userApi;
    private final CreateGroupMemberMapper createGroupMemberMapper;

    public CreateGroupMemberInOpenGroupUseCaseImpl(GroupMemberRepository groupMemberRepository, GroupRepository groupRepository,
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

        Optional<GroupMember> groupMemberOpt = groupMemberRepository.findByUserIdAndGroup(group, userInfoApiDto.userId());

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
