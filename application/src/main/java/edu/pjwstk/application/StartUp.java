package edu.pjwstk.application;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

@EnableJpaRepositories(basePackages = "edu.pjwstk")
@EntityScan(basePackages = "edu.pjwstk")
@SpringBootApplication(
        scanBasePackages = "edu.pjwstk"
)
public class StartUp {

    public static void main(String[] args) {
        SpringApplication.run(StartUp.class, args);
    }

}
