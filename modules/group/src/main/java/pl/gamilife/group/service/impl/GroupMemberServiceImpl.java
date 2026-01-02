package pl.gamilife.group.service.impl;

import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.BasicUserInfoDto;
import pl.gamilife.group.exception.domain.GroupFullException;
import pl.gamilife.group.exception.domain.UserAlreadyMemberOfGroupException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.group.service.GroupMemberService;
import pl.gamilife.shared.kernel.event.JoinedGroupEvent;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;

import java.util.Optional;
import java.util.UUID;

@Service
@AllArgsConstructor
public class GroupMemberServiceImpl implements GroupMemberService {

    private final GroupMemberJpaRepository groupMemberRepository;
    private final ApplicationEventPublisher eventPublisher;
    private final UserApi userApi;

    @Override
    @Transactional
    public GroupMember createGroupMember(Group group, UUID userId) {
        if (group.isFull()) {
            throw new GroupFullException("Group with id: " + group.getId() + " is full!");
        }

        var currentMembersUserIds = group.getActiveMembers()
                .stream()
                .map(GroupMember::getUserId)
                .toList();

        BasicUserInfoDto user = userApi.getUserById(userId).orElseThrow(
                () -> new UserNotFoundException("User with id: " + userId + " not found!")
        );

        Optional<GroupMember> groupMemberOpt = groupMemberRepository
                .findByUserIdAndGroup(userId, group);
        GroupMember groupMember;
        boolean wasPreviouslyMember = groupMemberOpt.isPresent();
        if (wasPreviouslyMember) {
            groupMember = groupMemberOpt.get();

            if (groupMember.isActive()) {
                throw new UserAlreadyMemberOfGroupException("User with id: " + userId +
                        " is already member of group with id: " + group.getId()
                );
            }

            groupMember = reactivatePreviousMember(groupMember);
        } else {
            groupMember = createNewGroupMember(userId, group);
        }

        eventPublisher.publishEvent(new JoinedGroupEvent(
                user.userId(),
                user.username(),
                wasPreviouslyMember,
                group.getId(),
                group.getName(),
                currentMembersUserIds
        ));

        return groupMember;
    }

    private GroupMember reactivatePreviousMember(GroupMember groupMember) {
        groupMember.rejoin();
        return groupMemberRepository.save(groupMember);
    }

    private GroupMember createNewGroupMember(UUID userId, Group group) {
        GroupMember newGroupMember = GroupMember.create(group, userId);
        return groupMemberRepository.save(newGroupMember);
    }
}
