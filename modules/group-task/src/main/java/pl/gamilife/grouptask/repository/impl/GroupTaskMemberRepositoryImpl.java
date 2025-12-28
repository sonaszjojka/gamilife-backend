package pl.gamilife.grouptask.repository.impl;

import org.springframework.stereotype.Repository;
import pl.gamilife.grouptask.entity.GroupTaskMember;
import pl.gamilife.grouptask.repository.GroupTaskMemberRepository;
import pl.gamilife.grouptask.repository.jpa.GroupTaskMemberRepositoryJpa;

import java.util.Optional;
import java.util.UUID;

@Repository
public class GroupTaskMemberRepositoryImpl implements GroupTaskMemberRepository {
    private final GroupTaskMemberRepositoryJpa groupTaskMemberRepositoryJpa;

    public GroupTaskMemberRepositoryImpl(GroupTaskMemberRepositoryJpa groupTaskMemberRepositoryJpa) {
        this.groupTaskMemberRepositoryJpa = groupTaskMemberRepositoryJpa;
    }


    @Override
    public GroupTaskMember save(GroupTaskMember groupTaskMember) {
        return groupTaskMemberRepositoryJpa.save(groupTaskMember);
    }

    @Override
    public void deleteById(UUID groupTaskMemberId) {
        groupTaskMemberRepositoryJpa.deleteById(groupTaskMemberId);
    }

    @Override
    public boolean existsById(UUID groupTaskMemberId) {
        return groupTaskMemberRepositoryJpa.existsById(groupTaskMemberId);
    }

    @Override
    public Optional<GroupTaskMember> findByGroupTaskMemberId(UUID groupTaskMemberId) {
        return groupTaskMemberRepositoryJpa.findById(groupTaskMemberId);
    }

}
