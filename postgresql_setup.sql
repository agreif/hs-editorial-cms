-- gen triggers - start




drop function public.process_audit_user() cascade;
create or replace function public.process_audit_user()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('user_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into user_history
                       (id, ident, password, email, is_admin, is_editor, is_reviewer, is_author, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.ident, new.password, new.email, new.is_admin, new.is_editor, new.is_reviewer, new.is_author, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_user after insert or update on public.user for each row execute procedure public.process_audit_user();



drop function public.process_audit_config() cascade;
create or replace function public.process_audit_config()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('config_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into config_history
                       (id, code, string_value, int_value, double_value, bool_value, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.code, new.string_value, new.int_value, new.double_value, new.bool_value, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_config after insert or update on public.config for each row execute procedure public.process_audit_config();




drop function public.process_audit_rawdata() cascade;
create or replace function public.process_audit_rawdata()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('rawdata_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into rawdata_history
                       (id, bytes, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.bytes, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_rawdata after insert or update on public.rawdata for each row execute procedure public.process_audit_rawdata();



drop function public.process_audit_submission() cascade;
create or replace function public.process_audit_submission()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('submission_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into submission_history
                       (id, issue_id, headline, subline, text, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.issue_id, new.headline, new.subline, new.text, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_submission after insert or update on public.submission for each row execute procedure public.process_audit_submission();



drop function public.process_audit_submissionfile() cascade;
create or replace function public.process_audit_submissionfile()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('submissionfile_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into submissionfile_history
                       (id, submission_id, rawdata_id, filename, mimetype, size, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.submission_id, new.rawdata_id, new.filename, new.mimetype, new.size, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_submissionfile after insert or update on public.submissionfile for each row execute procedure public.process_audit_submissionfile();





drop function public.process_audit_issue() cascade;
create or replace function public.process_audit_issue()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('issue_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into issue_history
                       (id, name, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.name, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_issue after insert or update on public.issue for each row execute procedure public.process_audit_issue();





drop function public.process_audit_rubric_type() cascade;
create or replace function public.process_audit_rubric_type()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('rubric_type_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into rubric_type_history
                       (id, name, sort_index, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.name, new.sort_index, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_rubric_type after insert or update on public.rubric_type for each row execute procedure public.process_audit_rubric_type();

-- gen triggers - end
