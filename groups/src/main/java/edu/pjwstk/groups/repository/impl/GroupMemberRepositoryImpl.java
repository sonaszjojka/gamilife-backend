package edu.pjwstk.groups.repository.impl;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.groups.repository.jpa.GroupMemberRepositoryJpa;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public class GroupMemberRepositoryImpl implements GroupMemberRepository {

    private final GroupMemberRepositoryJpa repositoryJpa;

    public GroupMemberRepositoryImpl(GroupMemberRepositoryJpa repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public Optional<GroupMember> findById(UUID groupMemberId) {
        return repositoryJpa.findById(groupMemberId);
    }

    @Override
    public boolean existsByUserIdAndGroup(Group group, UUID userId) {
        return repositoryJpa.existsByUserIdAndMemberGroup(userId, group);
    }

    @Override
    public GroupMember save(GroupMember groupMember) {
        return repositoryJpa.save(groupMember);
    }

    @Override
    public Integer countByMemberGroup(Group group) {
        return repositoryJpa.countByMemberGroup(group);
    }

    @Override
    public Optional<GroupMember> findByUserIdAndGroup(Group group, UUID uuid) {
        return repositoryJpa.findByUserIdAndMemberGroup(uuid, group);
    }

}
