package edu.pjwstk.groups.service.impl;

import edu.pjwstk.groups.exception.domain.GroupFullException;
import edu.pjwstk.groups.exception.domain.UserAlreadyMemberOfGroupException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import edu.pjwstk.groups.service.GroupMemberService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.UUID;

@Service
@AllArgsConstructor
public class GroupMemberServiceImpl implements GroupMemberService {

    private final GroupMemberJpaRepository groupMemberRepository;

    @Override
    public GroupMember createGroupMember(Group group, UUID userId) {
        if (group.isFull()) {
            throw new GroupFullException("Group with id: " + group.getGroupId() + " is full!");
        }

        Optional<GroupMember> groupMemberOpt = groupMemberRepository
                .findByUserIdAndGroup(userId, group);
        if (groupMemberOpt.isPresent()) {
            GroupMember groupMember = groupMemberOpt.get();

            if (groupMember.isActive()) {
                throw new UserAlreadyMemberOfGroupException("User with id: " + userId +
                        " is already member of group with id: " + group.getGroupId()
                );
            }

            return reactivatePreviousMember(groupMember);
        }

        return createNewGroupMember(userId, group);
    }

    private GroupMember reactivatePreviousMember(GroupMember groupMember) {
        groupMember.setLeftAt(null);

        return groupMemberRepository.save(groupMember);
    }

    private GroupMember createNewGroupMember(UUID userId, Group group) {
        GroupMember newGroupMember = GroupMember.builder()
                .groupMemberId(UUID.randomUUID())
                .group(group)
                .userId(userId)
                .leftAt(null)
                .totalEarnedMoney(0)
                .groupMoney(0)
                .build();

        return groupMemberRepository.save(newGroupMember);
    }
}
