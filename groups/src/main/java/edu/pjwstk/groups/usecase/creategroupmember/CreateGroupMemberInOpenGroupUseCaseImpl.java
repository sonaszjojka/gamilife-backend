package edu.pjwstk.groups.usecase.creategroupmember;

import edu.pjwstk.common.groupsApi.exception.GroupNotFoundException;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.exception.UserNotFoundException;
import edu.pjwstk.groups.entity.*;
import edu.pjwstk.groups.exception.*;
import edu.pjwstk.groups.repository.*;
import edu.pjwstk.groups.shared.GroupTypeEnum;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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

        if (groupMemberRepository.existsByUserIdAndGroup(group, userInfoApiDto.userId())) {
            throw new UserAlreadyMemberOfGroupException("User with id: " + userInfoApiDto.userId()
                    + " is already member of group with id: " + groupId + "!");
        }

        if (group.getGroupMembers().size() >= group.getMembersLimit()) {
            throw new GroupFullException("Group with id: " + groupId + " is full!");
        }

        if (group.getGroupType().toEnum() == GroupTypeEnum.CLOSED
                || group.getGroupType().toEnum() == GroupTypeEnum.REQUEST_ONLY) {
            throw new UserJoinGroupAccessDeniedException("To add user to group which type is: REQUEST_ONLY or CLOSED " +
                    " - invitation or request must be accepted.");
        }


        GroupMember groupMember = createGroupMemberMapper.toEntity(userInfoApiDto.userId(), group);
        GroupMember savedGroupMember = groupMemberRepository.save(groupMember);
        return createGroupMemberMapper.toResponse(savedGroupMember);
    }
}
