package edu.pjwstk.grouptasks.repository.impl;

import edu.pjwstk.grouptasks.repository.jpa.GroupTaskRepositoryJpa;
import org.springframework.stereotype.Repository;

@Repository
public class GroupTaskRepositoryImpl {
    private final GroupTaskRepositoryJpa groupTaskRepositoryJpa;

    public GroupTaskRepositoryImpl(GroupTaskRepositoryJpa groupTaskRepositoryJpa) {
        this.groupTaskRepositoryJpa = groupTaskRepositoryJpa;
    }
}
