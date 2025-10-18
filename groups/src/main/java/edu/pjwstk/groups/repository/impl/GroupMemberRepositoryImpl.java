package edu.pjwstk.groups.repository.impl;

import edu.pjwstk.groups.domain.GroupMember;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.groups.repository.jpa.GroupMemberRepositoryJpa;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public class GroupMemberRepositoryImpl implements GroupMemberRepository {

    private final GroupMemberRepositoryJpa repositoryJpa;

    public GroupMemberRepositoryImpl(GroupMemberRepositoryJpa repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public Optional<GroupMember> findById(Integer groupMemberId) {
        return repositoryJpa.findById(groupMemberId);
    }
}
