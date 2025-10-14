package edu.pjwstk.grouptasks.repository.impl;

import edu.pjwstk.grouptasks.repository.jpa.GroupTaskMemberRepositoryJpa;
import org.springframework.stereotype.Repository;

@Repository
public class GroupTaskMemberRepositoryImpl {
    private final GroupTaskMemberRepositoryJpa groupTaskMemberRepositoryJpa;

    public GroupTaskMemberRepositoryImpl(GroupTaskMemberRepositoryJpa groupTaskMemberRepositoryJpa) {
        this.groupTaskMemberRepositoryJpa = groupTaskMemberRepositoryJpa;
    }


}
