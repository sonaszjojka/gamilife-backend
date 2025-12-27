package pl.gamilife.group.service.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.group.exception.domain.GroupFullException;
import pl.gamilife.group.exception.domain.UserAlreadyMemberOfGroupException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.group.service.GroupMemberService;

import java.util.Optional;
import java.util.UUID;

@Service
@AllArgsConstructor
public class GroupMemberServiceImpl implements GroupMemberService {

    private final GroupMemberJpaRepository groupMemberRepository;

    @Override
    @Transactional
    public GroupMember createGroupMember(Group group, UUID userId) {
        if (group.isFull()) {
            throw new GroupFullException("Group with id: " + group.getId() + " is full!");
        }

        Optional<GroupMember> groupMemberOpt = groupMemberRepository
                .findByUserIdAndGroup(userId, group);
        if (groupMemberOpt.isPresent()) {
            GroupMember groupMember = groupMemberOpt.get();

            if (groupMember.isActive()) {
                throw new UserAlreadyMemberOfGroupException("User with id: " + userId +
                        " is already member of group with id: " + group.getId()
                );
            }

            return reactivatePreviousMember(groupMember);
        }

        return createNewGroupMember(userId, group);
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
